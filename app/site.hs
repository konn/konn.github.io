{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Main where

import Blaze.ByteString.Builder (toByteString)
import Breadcrumb
import Control.Applicative ((<|>))
import Control.Concurrent (newChan, readChan, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (SomeException, try)
import Control.Lens
  ( both,
    iforM_,
    imap,
    ix,
    (%~),
    (.~),
    (^?),
    _2,
  )
import Control.Monad hiding (mapM)
import Control.Monad.State
import qualified Crypto.Hash.SHA256 as SHA
import Data.Aeson
  ( Options,
    ToJSON (..),
    Value (..),
    camelTo2,
    defaultOptions,
    fieldLabelModifier,
    fromJSON,
    genericToJSON,
    toJSON,
  )
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.CaseInsensitive as CI
import Data.Char hiding (Space)
import Data.Data (Data)
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.Function
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe
  ( fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    maybeToList,
  )
import Data.Ord (Down (..))
import qualified Data.Store as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as ET
import qualified Data.Text.ICU.Normalize as UNF
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lens (unpacked)
import Data.Time
import Data.Yaml (object, (.=))
import GHC.Generics
import Instances ()
import Language.Haskell.TH (litE, runIO, stringL)
import Lenses
import Macro
import MathConv
import MissingSake
import Network.HTTP.Types
import Network.URI
import Network.Wai.Application.Static
  ( defaultWebAppSettings,
    ssIndices,
    staticApp,
  )
import Network.Wai.Handler.Warp (run)
import PubTalks
import Settings
import Skylighting hiding (Context (..), ListItem (..), Style)
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    getHomeDirectory,
    getModificationTime,
    renameFile,
  )
import System.Environment
import System.FSNotify (watchTreeChan, withManager)
import System.FilePath.Posix
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as H5 hiding (span)
import Text.Blaze.Internal (Attributable)
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup as TS
import Text.HTML.TagSoup.Match
import Text.Hamlet
import Text.LaTeX.Base (render)
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax hiding ((<>))
import qualified Text.Mustache as Mus
import Text.Pandoc hiding (getModificationTime, runIO)
import Text.Pandoc.Builder hiding ((<>))
import qualified Text.Pandoc.Builder as Pan
import Text.Pandoc.Citeproc
import Text.Pandoc.Shared (stringify, trim)
import qualified Text.TeXMath as UM
import Utils
import WaiAppStatic.Types (unsafeToPiece)
import Web.Sake hiding (Env)
import Web.Sake.Conf
import Web.Sake.Feed
import Web.Sake.Html
import Prelude hiding (mapM)
import qualified Prelude as P

default (String, T.Text)

home :: FilePath
home = $(litE . stringL =<< runIO getHomeDirectory)

{- globalBib :: FilePath
globalBib = home </> "Library/texmf/bibtex/bib/myreference.bib"
 -}
myShakeOpts :: ShakeOptions
myShakeOpts =
  shakeOptions
    { shakeThreads = 0
    , shakeProgress = progressSimple
    , shakeChange = ChangeDigest
    }

templateDir :: T.Text
templateDir = "templae"

siteConf :: SakeConf
destD :: FilePath
srcD :: FilePath
cacheD :: FilePath
siteConf@SakeConf
  { destinationDir = destD
  , sourceDir = srcD
  , cacheDir = cacheD
  } =
    Text.Pandoc.def
      { destinationDir = "_site"
      , sourceDir = "site-src"
      , cacheDir = "_cache"
      , ignoreFile = \fp ->
          or
            [ "~" `L.isSuffixOf` fp
            , "#" `L.isSuffixOf` fp && "#" `L.isPrefixOf` fp
            , "//.DS_Store" ?== fp
            ]
      }

destToSrc :: FilePath -> FilePath
destToSrc = replaceDir (destinationDir siteConf) (sourceDir siteConf)

articleList :: FilePath
articleList = "_build" </> ".articles"

main :: IO ()
main =
  getArgs >>= \case
    "watch" : args -> watch args
    args -> withArgs args runShake

watch :: [String] -> IO ()
watch args = do
  chan <- newChan
  server `concurrently_` watcher chan `concurrently_` builder chan
  where
    server =
      run 8000 $
        staticApp $
          (defaultWebAppSettings destD)
            { ssIndices = [unsafeToPiece "index.html"]
            }
    builder chan = do
      try @SomeException $ withArgs args runShake
      forever $ do
        _ <- readChan chan
        putStrLn "Changed!"
        withArgs args runShake
        putStrLn "Compilation Finished."
    watcher chan = withManager $ \man -> do
      src <- canonicalizePath srcD
      watchTreeChan man src (const True) chan
      forever $ threadDelay maxBound

copies :: [Patterns]
copies =
  [ "t//*"
  , "js//*"
  , ".well-known//*"
  , "//*imgs//*"
  , "//*img//*"
  , "math/cellular-automaton//*"
  , "math/computational-algebra-seminar//*"
  , "prog/automaton//*"
  , "prog/doc//*"
  , "math//*.pdf"
  , "prog/ruby//*"
  , "css//*.css"
  , "css//*.map"
  , "//*.key"
  , "//*.webp"
  , "logs//*.jpg"
  , "logs//*.png"
  , "math/3d-mandelbrot/*"
  , "katex//*"
  , "keybase.txt"
  , "math/cellular-automaton//*.png"
  , "//*.jpg"
  , "//*.gif"
  , "//*.webm"
  ]

runShake :: IO ()
runShake = shakeArgs myShakeOpts $ do
  want ["site"]

  let routing =
        [ Convert ("//*.md" .||. "//*.html" .||. "//*.tex") (-<.> "html")
        , Convert "//*.tex" (-<.> "pdf")
        , Convert "//*.sass" (-<.> "css")
        , Convert "robots.txt" id
        , Cached "//*.tex" (<.> "preprocess")
        ]
          ++ map Copy copies
          ++ [Create "feed.xml", Create "sitemap.xml", Create ".ignore"]

  "build" ~> need ["site"]

  "deploy" ~> do
    cmd_ (Cwd "_site") "git" "commit" "-amupdated"
      `actionCatch` \(_ :: SomeException) -> pure ()
    cmd_ (Cwd "_site") "git" "push"
      `actionCatch` \(_ :: SomeException) -> pure ()
    () <-
      cmd_ "git" "add" "templates" "config" "site-src" "app"
        `actionCatch` \(_ :: SomeException) -> pure ()
    () <-
      cmd_ "git" "commit" "-amupdated"
        `actionCatch` \(_ :: SomeException) -> pure ()
    cmd_ "git" "push" "origin" "master"

  withRouteRules siteConf routing $ do
    map (destD </>) ["prog/automaton", "prog/doc//*", "prog/ruby//*"] |%> \out -> do
      orig <- getSourcePath siteConf out
      copyFile' orig out

    (destD </> ".ignore") %> \out -> do
      drafts <- listDrafts destD
      writeFile' out $ unlines $ ".ignore" : mapMaybe (stripPrefix destD . itemTarget) drafts

    (destD <//> "*.css") %> \out -> do
      origPath <- getSourcePath siteConf out
      need [origPath]
      if ".sass" `L.isSuffixOf` origPath
        then
          cmd_
            (EchoStdout False)
            (WithStdout True)
            "sassc"
            "-tcompressed"
            (FileStdin origPath)
            (FileStdout out)
        else
          cmd_
            (EchoStdout False)
            (WithStdout True)
            "yuicompressor"
            origPath
            "-o"
            out

    (destD </> "robots.txt") %> \out -> do
      tmpl <- itemBody <$> loadItemWith out (srcD </> "robots.txt")
      drafts <- mapMaybe (stripDirectory destD . itemTarget) <$> listDrafts out
      let obj = object ["disallowed" .= map ('/' :) drafts]
      writeLazyTextFile out $ Mus.renderMustache tmpl obj

    (destD </> "sitemap.xml") %> \out -> do
      items <- filter isPublished <$> listChildren True out
      let itemsCtx = urlField <> updatedDateField "date" "%Y-%m-%d" <> defaultContext
          ctx =
            mconcat
              [ defaultContext
              , listField
                  "items"
                  itemsCtx
                  items
              ]
      writeTextFile out . itemBody =<< applyAsMustache ctx
        =<< loadItem "templates/sitemap.mustache"

    (destD </> "archive.html") %> \out -> do
      (count, items) <-
        renderPostList
          =<< myRecentFirst
          =<< listChildren False out
      let ctx =
            mconcat
              [ constField "child-count" count
              , constField "children" items
              , myDefaultContext
              ]
      loadItemWith out (srcD </> "archive.md")
        >>= compilePandoc readerConf writerConf
        >>= applyDefaultTemplate ctx {- tags -}
        >>= writeTextFile out . itemBody

    (destD </> "math//*.pdf") %> \out -> do
      srcPath <- getSourcePath siteConf out
      let texFileName = takeFileName srcPath
          bibPath = srcPath -<.> "bib"
      if ".pdf" `L.isSuffixOf` srcPath
        then copyFile' srcPath out
        else do
          Item {..} <- loadItemWith out $ destToSrc $ out -<.> "tex"
          let opts =
                fromMaybe "-pdflua" $
                  maybeResult . fromJSON =<< KM.lookup "latexmk" itemMetadata
          withTempDir $ \tmp -> do
            writeTextFile (tmp </> texFileName) itemBody
            bibThere <- doesFileExist bibPath
            when bibThere $ copyFile' bibPath (tmp </> takeFileName bibPath)
            copyFile' ("data" </> ".latexmkrc") (tmp </> ".latexmkrc")
            cmd_ "latexmk" (EchoStdout False) (WithStdout True) (Cwd tmp) opts texFileName
            copyFileNoDep (tmp </> texFileName -<.> "pdf") out

    (destD </> "logs" </> "index.html") %> \out -> do
      logs <-
        mapM toLog
          =<< myRecentFirst
          =<< loadAllSnapshots
            siteConf
            "content"
              { snapshotSource = "logs//*.md" .&&. complement "logs/index.md"
              }
      let pages = L.chunksOf 5 logs
          toName 0 = out
          toName n = dropExtension out ++ "-" ++ show n <.> "html"
      tmpl <- myPandocCompiler out
      makePagination mempty tmpl toName pages

    (destD </> "index.html") %> \out -> do
      (count, posts) <-
        renderPostList . take 5
          =<< aggregateLogs
          =<< myRecentFirst
          =<< listChildren False out
      let ctx =
            mconcat
              [ constField "child-count" (show count)
              , constField "updates" posts
              , myDefaultContext
              ]
      let mdPath = destToSrc $ out -<.> "md"
      mdThere <- doesFileExist mdPath
      let pth
            | mdThere = mdPath
            | otherwise = mdPath -<.> "html"
      loadItemWith out pth
        >>= compilePandoc readerConf writerConf
        >>= applyDefaultTemplate ctx {- tags -}
        >>= writeTextFile out . itemBody

    fromString (destD <//> "index.html") .&&. complement (disjoin copies) %%> \out -> do
      (count, chl) <-
        renderPostList
          =<< myRecentFirst
          =<< listChildren False out
      let ctx =
            mconcat
              [ constField "child-count" (show count)
              , constField "children" chl
              , myDefaultContext
              ]
      writeTextFile out . itemBody
        =<< applyDefaultTemplate ctx
        =<< myPandocCompiler out

    destD <//> "profile.html" %> \out -> do
      talks <- renderTalks $ Just 10
      let cxt = constField "talks" $ itemBody talks
      mdOrHtmlToHtml' cxt out
    destD <//> "talks.html" %> \out -> do
      talks <- renderTalks Nothing
      let cxt = constField "talks" $ itemBody talks
      mdOrHtmlToHtml' cxt out

    fromString (destD <//> "*.html") .&&. complement (disjoin copies) %%> \out -> do
      srcPath <- getSourcePath siteConf out
      need [srcPath]
      if
          | ".tex" `L.isSuffixOf` srcPath -> texToHtml out
          | ".md" `L.isSuffixOf` srcPath -> mdOrHtmlToHtml out
          | ".html" `L.isSuffixOf` srcPath ->
            if (destD </> "articles") `L.isPrefixOf` out
              then mdOrHtmlToHtml out
              else copyFile' srcPath out
          | otherwise -> error $ "Unknown extension: " <> takeFileName srcPath
    (cacheD </> "talks.bin") %> \out -> do
      talks <- readFromYamlFile' @_ @[Talk] "site-src/talks.yaml"
      writeBinaryFile out talks

    (cacheD <//> "*.tex.preprocess") %> \out -> do
      macs <- readFromYamlFile' "config/macros.yml"
      let origPath = replaceDir cacheD srcD $ dropExtension out
      i@Item {..} <- loadItemWith out origPath
      let cmacs = itemMacros i
          ans = preprocessLaTeX (cmacs <> macs) itemBody
          go = do
            forM_ (images ans) $ \(_, src) ->
              unless (T.null src) $ generateImages out src
            writeBinaryFile out ans
      b <- doesFileExist out
      if not b
        then go
        else do
          old <- SHA.hash <$> readBinaryFileNoDep out
          when (SHA.hash (S.encode ans) /= old) go

    (destD </> "feed.xml") %> \out -> do
      putNormal "Generating feed."
      loadAllSnapshots
        siteConf
        "content"
          { snapshotSource = ("//*.md" .||. "//*.html" .||. "//*.tex") .&&. complement ("draft//*" .||. "logs/index*.md" .||. "logs/index*.html")
          }
        >>= myRecentFirst
        >>= renderAtom feedConf feedCxt
          . map (fmap normaliseFeed)
          . take 10
          . filter ((complement ("//index.md" .||. "//archive.md") ?===) . itemPath)
        >>= writeTextFile out

    serialPngOrSvg ?> \out -> do
      let prepro = replaceDir destD cacheD $ takeDirectory out <.> "tex" <.> "preprocess"
      need [prepro]

serialPngOrSvg :: FilePath -> Bool
serialPngOrSvg fp =
  (fromString (destD </> "math//*.svg") .||. fromString (destD </> "math//*.png")) ?=== fp
    && isJust (takeImageNumber fp)

takeImageNumber :: FilePath -> Maybe Int
takeImageNumber fp = do
  let fname = takeFileName fp
  l0 <- L.stripPrefix "image-" fname
  readMaybe $ dropExtension l0

texToHtml :: FilePath -> Action ()
texToHtml out = do
  i0 <- loadItemWith @_ @T.Text out $ destToSrc $ out -<.> "tex"
  PreprocessedLaTeX {..} <- readFromBinaryFile' (replaceDir srcD cacheD (itemPath i0) <.> "preprocess")
  let imgs =
        maybe
          []
          ( \(j, _) ->
              concat
                [ [base <.> "png", base <.> "svg"]
                | i <- [0 .. j - 1]
                , let base = dropExtension out </> "image-" ++ show i
                ]
          )
          images
  putNormal $ "requiring images: " ++ show imgs
  needed $ (out -<.> "pdf") : imgs
  (style, bibs) <- cslAndBib out
  defMacs <- readFromYamlFile' "config/macros.yml"
  ipan <-
    linkCard . addPDFLink ("/" </> dropDirectory1 out -<.> "pdf")
      . addAmazonAssociateLink "konn06-22"
      =<< procSchemes
      =<< myProcCites style bibs
      =<< liftIO (texToMarkdown defMacs out latexSource)
  let html = "{{=<% %>=}}\n" <> fromPure (Text.Pandoc.writeHtml5String writerConf ipan)
      panCtx =
        pandocContext $
          ipan & _Pandoc . _2 %~ (Text.Pandoc.RawBlock "html" "{{=<% %>=}}\n" :)
  writeTextFile out . itemBody
    =<< applyDefaultTemplate panCtx . fmap ("{{=<% %>=}}\n" <>)
    =<< applyAsMustache panCtx (setItemBody html i0)

mdOrHtmlToHtml :: FilePath -> Action ()
mdOrHtmlToHtml = mdOrHtmlToHtml' mempty

mdOrHtmlToHtml' :: Context T.Text -> FilePath -> Action ()
mdOrHtmlToHtml' cxt out =
  myPandocCompiler out
    >>= applyDefaultTemplate (cxt <> myDefaultContext)
    >>= writeTextFile out . itemBody

type CSLPath = FilePath

newtype RefMeta = RefMeta Text.Pandoc.Meta
  deriving newtype (Semigroup, Monoid)

instance Readable RefMeta where
  readFrom_ fp = Text.Pandoc.runIOorExplode do
    fmap (\(Text.Pandoc.Pandoc m _) -> RefMeta m) . Text.Pandoc.readBibLaTeX Text.Pandoc.def
      =<< liftIO (T.readFile fp)

cslAndBib :: FilePath -> Action (CSLPath, RefMeta)
cslAndBib fp = do
  let cslPath = srcD </> fp -<.> "csl"
  {-  bibPath = srcD </> fp -<.> "bib"
  mbib <-
    tryWithFile bibPath $
      readFromFile' bibPath -}
  -- gbib <- readFromFile' globalBib
  -- need [globalBib]
  customCSL <- doesFileExist cslPath
  let finalCslPath
        | customCSL = cslPath
        | otherwise = "data" </> "default.csl"
  let bibs = mempty -- fromMaybe mempty mbib <> gbib
  return (finalCslPath, bibs)

pandocContext :: Text.Pandoc.Pandoc -> Context a
pandocContext (Text.Pandoc.Pandoc meta _)
  | Just abst <- Text.Pandoc.lookupMeta "abstract" meta =
    constField "abstract" $
      T.unpack $
        fromPure $
          Text.Pandoc.writeHtml5String writerConf $ Text.Pandoc.Pandoc meta (mvToBlocks abst)
  | otherwise = mempty

listDrafts :: FilePath -> Action [Item T.Text]
listDrafts fp =
  filter (not . isPublished) <$> listChildren False fp

addPDFLink :: FilePath -> Text.Pandoc.Pandoc -> Text.Pandoc.Pandoc
addPDFLink plink (Text.Pandoc.Pandoc meta body) = Text.Pandoc.Pandoc meta body'
  where
    Text.Pandoc.Pandoc _ body' =
      doc $
        mconcat
          [ para $ mconcat ["[", link' plink "PDF版" "PDF版", "]"]
          , Pan.fromList body
          ]

link' :: (H5.ToMarkup a2, H5.ToMarkup a1) => a1 -> a2 -> p -> Inlines
link' l txt _ttl =
  rawInline "html" $
    LT.toStrict $
      renderHtml
        [shamlet|<a href=#{l} onclick="ga('send', 'event', 'download', 'ac', '#{l}')">#{txt}|]

listChildren :: Bool -> FilePath -> Action [Item T.Text]
listChildren includeIndices out = do
  let dir = takeDirectory $ destToSrc out
      excludes =
        disjoin $
          fromString out :
          concat [["//index.md", "archive.md", "archive.md", "//index.md"] | not includeIndices] ++ copies
  mapM loadItemToHtml . filter (not . (?===) excludes . dropDirectory1) . map (dir </>)
    =<< globDirectoryFiles dir subContentsWithoutIndex

loadItemToHtml :: (Readable a, MonadSake f) => FilePath -> f (Item a)
loadItemToHtml fp = loadItemWith (replaceDir srcD destD fp -<.> "html") fp

subContentsWithoutIndex :: Patterns
subContentsWithoutIndex =
  ("*//*.html" .||. "//*.md" .||. "//*.tex")
    .&&. complement
      ( disjoin
          [ "prog/doc//*"
          , "prog/ruby//*"
          , "prog/automaton//*"
          , "math/cellular-automaton//*"
          , "t//*"
          , "math/computational-algebra-seminar//*"
          ]
      )

feedCxt :: Context T.Text
feedCxt =
  mconcat
    [ field "published" itemDateFeedStr
    , urlField
    , field "updated" itemDateFeedStr
    , bodyField "description"
    , defaultContext
    ]

normaliseFeed :: T.Text -> T.Text
normaliseFeed = concatMapTags $ \case
  TagOpen "iframe" _ -> []
  TagClose "iframe" -> []
  TagOpen op atts -> [TagOpen op $ filter (not . T.isPrefixOf "data-" . fst) atts]
  t -> [t]

itemDateFeedStr :: MonadSake m => Item a -> m T.Text
itemDateFeedStr = fmap (insertColon . T.pack . formatTime timeLocaleWithJST "%Y-%m-%dT%H:%M:%S%z") . itemDate
  where
    insertColon txt =
      let (bh, ah) = T.splitAt (T.length txt - 2) txt
       in bh <> ":" <> ah

itemDateStr :: MonadSake m => Item a -> m T.Text
itemDateStr = fmap (T.pack . formatTime timeLocaleWithJST "%Y/%m/%d %X %Z") . itemDate

feedConf :: FeedConf
feedConf =
  FeedConf
    { feedTitle = "konn-san.com 建設予定地"
    , feedDescription = "数理論理学を中心に数学、Haskell、推理小説、評論など。"
    , feedAuthor = FeedAuthor {authorName = "Hiromi ISHII", authorEmail = "konn.jinro_at_gmail.com"}
    , feedRoot = "https://konn-san.com"
    , feedUrl = "/feed.xml"
    }

writerConf :: Text.Pandoc.WriterOptions
writerConf =
  Text.Pandoc.def
    { -- We can't change this to KaTeX,
      -- as this sacrifices the conversion logic.
      writerHTMLMathMethod = Text.Pandoc.MathJax "https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.css"
    , writerHighlightStyle = Just pygments
    , writerSectionDivs = True
    , writerExtensions = Text.Pandoc.disableExtension Text.Pandoc.Ext_tex_math_dollars myExts
    }

readerConf :: Text.Pandoc.ReaderOptions
readerConf = Text.Pandoc.def {readerExtensions = myExts}

myPandocCompiler :: FilePath -> Action (Item T.Text)
myPandocCompiler out = do
  (csl, bib) <- cslAndBib out
  loadOriginal siteConf out
    >>= readPandoc readerConf
    >>= mapM
      ( procSchemes <=< myProcCites csl bib
          <=< linkCard . addAmazonAssociateLink "konn06-22"
      )
    >>= writePandoc writerConf

resolveRelatives :: FilePath -> FilePath -> FilePath
resolveRelatives rt pth =
  let revRoots = reverse $ splitPath rt
   in go revRoots $ splitPath pth
  where
    go _ ("/" : rest) = go [] rest
    go (_ : rs) ("../" : rest) = go rs rest
    go [] ("../" : rest) = go [".."] rest
    go r ("./" : rest) = go r rest
    go rs (fp : rest) = go (fp : rs) rest
    go fps [] = joinPath $ reverse fps

applyDefaultTemplate :: Context T.Text -> Item T.Text -> Action (Item T.Text)
applyDefaultTemplate addCtx item@Item {..} = do
  nav <- makeNavBar itemIdentifier
  let r = makeRelative destD $ itemPath item
      imgs =
        map (("https://konn-san.com/" <>) . resolveRelatives (takeDirectory r) . T.unpack) $
          extractLocalImages $ TS.parseTags itemBody
      navbar = constField "navbar" nav
      thumb =
        constField "thumbnail" $
          fromMaybe "https://konn-san.com/img/myface_mosaic.jpg" $
            listToMaybe imgs
      unpublished = field "unpublished" $ return . not . isPublished
      date = field "date" itemDateStr
      -- toc = field "toc" $ return . buildTOC . readHtml' readerConf . T.pack . itemBody
      cxt =
        mconcat
          [ thumb
          , shortDescrCtx
          , unpublished
          , addCtx
          , navbar
          , breadcrumbContext "breadcrumb"
          , headElemsCtx
          , metaTags
          , date
          , noTopStar
          , myDefaultContext
          ]
  let item' = demoteHeaders . withTags addRequiredClasses <$> item
  scms <- readFromYamlFile' "config/schemes.yml"
  i'' <-
    mapM (procKaTeX . relativizeUrlsTo (dropDirectory1 itemTarget))
      =<< loadAndApplyMustache "templates/default.mustache" cxt
      =<< saveSnapshot siteConf "content"
      =<< applyAsMustache cxt item'
  saveSnapshot siteConf "katexed" i''
  return $ UNF.normalize UNF.NFC . addAmazonAssociateLink' "konn06-22" . procSchemesUrl scms <$> i''

generateImages :: FilePath -> T.Text -> Action ()
generateImages fp body = do
  master <- liftIO $ canonicalizePath $ replaceDir cacheD destD $ dropExtensions fp
  liftIO $ createDirectoryIfMissing True $ fromString master
  withTempDir $ \tmp -> do
    copyFile' ("data" </> ".latexmkrc") (tmp </> ".latexmkrc")
    writeTextFile (tmp </> "image.tex") body
    cmd_ (Cwd tmp) "latexmk" (EchoStdout False) (WithStdout True) "-pdflua" "image.tex"
    cmd_
      (Cwd tmp)
      "tex2img"
      (EchoStdout False)
      (WithStdout True)
      ["--latex=lualatex"]
      "--with-text"
      "image.tex"
      "image.svg"
    -- Generating PNGs
    cmd_
      (Cwd tmp)
      (EchoStdout False)
      (WithStdout True)
      "convert"
      "-density"
      "200"
      "-strip"
      "image.pdf"
      "image-%d.png"
    Stdout infos <-
      cmd
        (Cwd tmp)
        "pdftk"
        (EchoStdout False)
        (WithStdout True)
        "image.pdf"
        "dump_data_utf8"
    let pages =
          fromMaybe (0 :: Integer) $
            listToMaybe $
              mapMaybe
                (readMaybe <=< L.stripPrefix "NumberOfPages: ")
                (lines infos)
    forM_ [1 .. pages - 1] $ \n -> do
      let targ = tmp </> "image-" <> show n <.> "svg"
      liftIO $ renameFile (tmp </> "image-" <> show (n + 1) <.> "svg") targ
    liftIO $ renameFile (tmp </> "image.svg") (tmp </> "image-0.svg")
    imgs <- getDirectoryFiles tmp ["*.png", "*.svg"]
    forM_ imgs $ \i ->
      traced (unwords ["Copying", tmp </> i, "to", master </> i]) $
        copyFileNoDep (tmp </> i) (master </> i)
    putNormal . unwords . ("generated:" :) =<< getDirectoryFiles tmp ["*.png", "*.svg"]

relativizeUrlsTo :: FilePath -> T.Text -> T.Text
relativizeUrlsTo targ = withUrls $ unpacked %~ makeRelative targ

headElemsCtx :: Context a
headElemsCtx = field_ "head" $ \i ->
  let fp = runIdentifier $ itemIdentifier i
      m
        | "//math//*" ?== fp = LT.toStrict $ renderHtml [shamlet|<link rel="stylesheet" href="/css/math.css">|]
        | otherwise = ""
      h = fromMaybe "" $ lookupMetadata "head" i
   in T.unlines [m, h]

descriptionField :: String -> Context a
descriptionField key =
  field_ key $ fromMaybe "" . lookupMetadata "description"

shortDescrCtx :: Context a
shortDescrCtx = field_ "short_description" $ \i ->
  let descr = fromMaybe "" $ lookupMetadata "description" i
   in either (const "") (T.unpack . T.replace "\n" " ") $
        Text.Pandoc.runPure $
          Text.Pandoc.writePlain Text.Pandoc.def . Text.Pandoc.bottomUp unicodiseMath =<< Text.Pandoc.readMarkdown readerConf descr

noTopStar :: Context b
noTopStar = field_ "no-top-star" $ \i ->
  case lookupMetadata "top-star" i of
    Just False -> True
    _ -> False

metaTags :: Context a
metaTags = field "meta" $ \i -> do
  let [desc0, tags] = flip map ["description", "tag"] $ \k ->
        fromMaybe "" $ lookupMetadata k i
  let desc =
        fromPure $
          Text.Pandoc.writePlain
            writerConf
              { writerHTMLMathMethod = Text.Pandoc.PlainMath
              , writerWrapText = Text.Pandoc.WrapNone
              }
            =<< Text.Pandoc.readMarkdown readerConf (T.pack desc0)
  return $
    renderHtml $ do
      H5.meta ! H5.name "Keywords" ! H5.content (H5.toValue tags)
      H5.meta ! H5.name "description" ! H5.content (H5.toValue desc)

useKaTeX :: Item a -> Bool
useKaTeX item =
  fromMaybe True $ lookupMetadata "katex" item

procKaTeX :: MonadAction m => T.Text -> m T.Text
procKaTeX = liftAction . fmap T.pack . prerenderKaTeX . T.unpack

prerenderKaTeX :: String -> Action String
prerenderKaTeX src = do
  need ["prerender-katex" </> "prerender" <.> "js"]
  Stdout out <-
    cmd
      (Cwd "prerender-katex")
      "node"
      (EchoStdout False)
      (WithStdout True)
      (Stdin src)
      "prerender.js"
  return out

isExternal :: T.Text -> Bool
isExternal url = any (`T.isPrefixOf` url) ["http://", "https://", "//"]

extractLocalImages :: [Tag T.Text] -> [T.Text]
extractLocalImages ts =
  [ src
  | TagOpen t atts <- ts
  , at <- maybeToList $ lookup t [("img", "src"), ("object", "data")]
  , (a, src) <- atts
  , a == at
  , not $ isExternal src
  , T.takeEnd 4 src /= ".svg"
  ]

addRequiredClasses :: Tag T.Text -> Tag T.Text
addRequiredClasses (TagOpen "table" attr) = TagOpen "table" (("class", "table") : attr)
addRequiredClasses (TagOpen "blockquote" attr) = TagOpen "blockquote" (("class", "blockquote") : attr)
addRequiredClasses t = t

-- deploy :: t -> IO ExitCode
-- deploy _config = handle h $ shelly $ do
--   ign0 <- T.lines <$> readfile "_site/.ignore"
--   let (gign, ign) = unzip $ map parseIgnorance ign0
--   echo $ "ignoring: " <> T.intercalate "," ign
--   writefile ".git/info/exclude" $ T.unlines gign
--   run_ "rsync" $ "--delete-excluded" : "--checksum" : "-av" : map ("--exclude=" <>) ign
--               ++ ["_site/", "sakura-vps:~/mighttpd/public_html/"]
--   cmd "git" "add" "img" "math" "writing" "prog" "config"
--   cmd "git" "commit" "-amupdated"
--   cmd "git" "push" "origin" "master"

--   return ExitSuccess
--   where
--     h :: IOException -> IO ExitCode
--     h _ = return $ ExitFailure 1

uriAuthToString_ :: URIAuth -> String
uriAuthToString_ (URIAuth a b c) = concat [a, b, c]

procSchemes :: Text.Pandoc.Pandoc -> Action Text.Pandoc.Pandoc
procSchemes = Text.Pandoc.bottomUpM procSchemes0

procSchemesUrl :: Schemes -> T.Text -> T.Text
procSchemesUrl (Schemes dic) =
  withUrls $ \u ->
    case parseURI $ T.unpack u of
      Just URI {..}
        | Just Scheme {..} <- HM.lookup (T.init $ T.pack uriScheme) dic ->
          let body =
                mconcat
                  [ maybe "" uriAuthToString_ uriAuthority
                  , uriPath
                  , uriQuery
                  , uriFragment
                  ]
           in prefix <> T.pack body <> fromMaybe "" postfix
      _ -> u

procSchemes0 :: Text.Pandoc.Inline -> Action Text.Pandoc.Inline
procSchemes0 inl =
  case inl ^? linkUrl of
    Nothing -> return inl
    Just url -> do
      Schemes dic <- readFromYamlFile' "config/schemes.yml"
      let url' =
            fromMaybe url $
              asum $
                imap
                  ( \k v ->
                      sandwitched (prefix v) (fromMaybe "" $ postfix v)
                        <$> T.stripPrefix (k <> ":") url
                  )
                  dic
      return $ inl & linkUrl .~ url'
  where
    sandwitched s e t = s <> t <> e

addAmazonAssociateLink :: T.Text -> Text.Pandoc.Pandoc -> Text.Pandoc.Pandoc
addAmazonAssociateLink = Text.Pandoc.bottomUp . procAmazon

addAmazonAssociateLink' :: T.Text -> T.Text -> T.Text
addAmazonAssociateLink' tag = withUrls (attachTo tag)

procAmazon :: T.Text -> Text.Pandoc.Inline -> Text.Pandoc.Inline
procAmazon tag (Text.Pandoc.Link atts is (url, ttl)) = Text.Pandoc.Link atts is (attachTo tag url, ttl)
procAmazon tag (Text.Pandoc.Image atts is (url, ttl)) = Text.Pandoc.Image atts is (attachTo tag url, ttl)
procAmazon _ il = il

attachTo :: T.Text -> T.Text -> T.Text
attachTo key url
  | (p@("http:" : "" : amazon : paths), qs) <- decodePath (ET.encodeUtf8 url)
    , amazon `elem` amazons
    , let cipath = map CI.mk paths
    , ["o", "asin"] `isPrefixOf` cipath || "dp" `elem` cipath
        || ["gp", "product"] `isPrefixOf` cipath
    , isNothing (lookup "tag" qs) =
    T.tail $
      ET.decodeUtf8 $
        toByteString $
          encodePath p (("tag", Just $ ET.encodeUtf8 key) : qs)
attachTo _ url = url

amazons :: [T.Text]
amazons = "www.amazon.com" : "amazon.com" : concatMap (\cc -> [T.concat [www, "amazon.", co, cc] | www <- ["", "www."], co <- ["co.", ""]]) ccTLDs

ccTLDs :: [T.Text]
ccTLDs = ["jp"]

getActive :: [(T.Text, String)] -> FilePath -> String
getActive _ "archive.md" = "/archive.html"
getActive _ "profile.md" = "/profile.html"
getActive cDic ident = fromMaybe "/" $ find p $ map snd cDic
  where
    p "/" = False
    p ('/' : inp) = fromString (inp ++ "//*") ?== ident
    p _ = False

makeNavBar :: Identifier -> Action String
makeNavBar ident = do
  NavBar cDic <- readFromYamlFile' "config/navbar.yml"
  let cats =
        toJSON
          [ object
            [ "path" .= pth
            , "category" .= cat
            , "active" .= (getActive cDic (dropDirectory1 $ runIdentifier ident) == pth)
            ]
          | (cat, pth) <- cDic
          ]
  src <- readFromFile' "templates/navbar.mustache"
  return $ LT.unpack $ Mus.renderMustache src cats

data Pagination = Pagination
  { paginationPrev :: Maybe Page
  , paginationNext :: Maybe Page
  , paginationPages :: [Page]
  }
  deriving (Read, Show, Eq, Ord, Generic)

paginationConf :: Options
paginationConf = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 10}

data Page = Page
  { pageLink :: String
  , pageNumber :: Int
  , pageActive :: Bool
  }
  deriving (Read, Show, Eq, Ord, Generic)

pageConf :: Options
pageConf = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}

instance ToJSON Page where
  toJSON = genericToJSON pageConf

instance ToJSON Pagination where
  toJSON = genericToJSON paginationConf

makePagination ::
  (ToJSON [a]) =>
  Context T.Text ->
  -- | page template
  Item T.Text ->
  -- | File name generator
  (Int -> FilePath) ->
  -- | items to be paginated (each list is accesible in field "items")
  [[Item a]] ->
  Action ()
makePagination ctx0 page toName iss = do
  tmpl <- readFromFile' "templates/pagination.mustache"
  let ps = map (\i -> Page (replaceDir destD "/" $ toName i) i False) [0 .. length iss - 1]
      pag i =
        Pagination
          { paginationPages = ps & ix i %~ \p -> p {pageActive = True}
          , paginationPrev =
              if i > 0
                then Just (ps !! (i - 1))
                else Nothing
          , paginationNext =
              if i < length ps - 1
                then Just (ps !! (i + 1))
                else Nothing
          }
  iforM_ iss $ \i is -> do
    let pagNav = Mus.renderMustache tmpl (toJSON $ pag i)
        ctx =
          mconcat
            [ constField "items" (map itemBody is)
            , constField "pagination" pagNav
            , ctx0
            , myDefaultContext
            ]
    writeTextFile (toName i) . itemBody
      =<< applyDefaultTemplate ctx page {itemTarget = toName i}

logPat :: Patterns
logPat = destD </?> "logs/*.html"

appendText :: Value -> Value -> Value
appendText ~(String a) ~(String b) = String (a <> b)

aggregateLogs :: [Item T.Text] -> Action [Item T.Text]
aggregateLogs is0 =
  case break ((logPat ?===) . itemTarget) is0 of
    (ps, a : qs) -> do
      i <- loadItem (srcD </> "logs/index.md")
      let rest = filter (not . (logPat ?===) . itemTarget) qs
          targ = itemTarget a
          anc = '#' : takeBaseName targ
          lastUpdate = T.concat ["（最終更新：", T.pack $ takeBaseName targ, "）"]
          meta' =
            itemMetadata i
              & insertWithKM (flip appendText) "description" (String lastUpdate)
              & KM.insert
                "date"
                (fromMaybe (String "") $ KM.lookup "date" (itemMetadata a))
          i' =
            i
              { itemTarget = destD </> "logs/index.html" ++ anc
              , itemMetadata = meta'
              }
      return $ ps ++ i' : rest
    (ps, qs) -> return $ ps ++ qs

insertWithKM :: (v -> v -> v) -> AK.Key -> v -> KM.KeyMap v -> KM.KeyMap v
insertWithKM f k v =
  runIdentity . flip KM.alterF k \case
    Nothing -> Identity $ Just v
    Just v' -> Identity $ Just $ f v' v

postList :: Maybe Int -> Action (Int, T.Text)
postList mcount =
  renderPostList . maybe id take mcount
    =<< myRecentFirst . filter isPublished
    =<< filterM (hasSnapshot siteConf "content")
    =<< listChildren False destD

urlField :: Context a
urlField = field_ "url" $ replaceDir destD "/" . itemTarget

renderPostList :: [Item T.Text] -> Action (Int, T.Text)
renderPostList posts = do
  postItemTpl <- readFromFile' "templates/update.mustache" :: Action Mustache
  let myDateField = field "date" itemDateStr
      pdfField = field "pdf" $ \Item {..} ->
        let fp = runIdentifier itemIdentifier
            pdfVer = replaceDir srcD destD fp -<.> "pdf"
         in if "//*.tex" ?== fp
              then do
                needed [pdfVer]
                return $ Just $ replaceDir destD "/" itemTarget -<.> "pdf"
              else return Nothing
      descField = field_ "description" $ \item ->
        let ident = itemIdentifier item
            descr = maybe "" T.pack $ lookupMetadata "description" item
            refs = buildRefInfo $ itemBody item
            fp = replaceDir srcD destD (runIdentifier ident) -<.> "html"
         in fromPure $
              Text.Pandoc.writeHtml5String writerConf . Text.Pandoc.bottomUp (remoteCiteLink (T.pack fp) refs)
                =<< Text.Pandoc.readMarkdown readerConf descr

      iCtxs =
        mconcat
          [ pdfField
          , myDateField
          , urlField
          , descField
          , defaultContext
          ]
  src <- procKaTeX =<< applyTemplateList postItemTpl iCtxs posts
  return (length posts, src)

myDefaultContext :: Context T.Text
myDefaultContext =
  mconcat [disqusCtx, defaultContext, canonicContext]
  where
    canonicContext = field "canonic" $ \item ->
      let targ = replaceDir destD "/" $ itemTarget item
       in return $
            if takeFileName targ == "index.html"
              then takeDirectory targ
              else targ
    blacklist = ["index.md", "//index.*", "archive.md", "profile.md"]
    disqusCtx = field "disqus" $ \item ->
      let fp = runIdentifier $ itemIdentifier item
          banned = any (?== fp) blacklist
          enabled = fromMaybe True $ lookupMetadata "disqus" item
       in return $ not banned && enabled

myRecentFirst :: [Item a] -> Action [Item a]
myRecentFirst is0 = do
  let is = filter isPublished is0
  ds <- mapM itemDate is
  return $ map snd $ sortOn (Down . zonedTimeToLocalTime . fst) $ zip ds is

isPublished :: Item a -> Bool
isPublished item =
  let pub = lookupMetadata "published" item
      dra = lookupMetadata "draft" item
   in fromMaybe True $ pub <|> (not <$> dra)

itemMacros :: Item a -> TeXMacros
itemMacros Item {..} =
  fromMaybe HM.empty $
    maybeResult . fromJSON =<< KM.lookup "macros" itemMetadata

capitalise :: String -> String
capitalise "" = ""
capitalise (c : cs) = toUpper c : map toLower cs

itemDate :: MonadSake m => Item a -> m ZonedTime
itemDate item =
  let ident = itemIdentifier item
      mdate =
        parseTimeM True timeLocaleWithJST "%Y/%m/%d %X %Z"
          =<< lookupMetadata "date" item
   in case mdate of
        Just date -> return date
        Nothing ->
          liftIO $
            utcToLocalZonedTime
              =<< getModificationTime (runIdentifier ident)

extractCites :: Data a => a -> [[Text.Pandoc.Citation]]
extractCites = Text.Pandoc.queryWith collect
  where
    collect (Text.Pandoc.Cite t _) = [t]
    collect _ = []

extractNoCites :: Data c => c -> [[Text.Pandoc.Citation]]
extractNoCites = Text.Pandoc.queryWith collect
  where
    collect (Text.Pandoc.RawInline "latex" src) =
      case parseLaTeX src of
        Left _ -> []
        Right t -> flip Text.Pandoc.queryWith t $ \case
          TeXComm "nocite" [cs] ->
            [ [ Text.Pandoc.Citation (trim w) [] [] Text.Pandoc.NormalCitation 0 0
              | w <- T.splitOn "," $ T.init $ T.tail $ render cs
              ]
            ]
          _ -> []
    collect _ = []

myProcCites :: CSLPath -> RefMeta -> Text.Pandoc.Pandoc -> Action Text.Pandoc.Pandoc
myProcCites style RefMeta {} p0 = do
  p <-
    liftIO $
      Text.Pandoc.runIOorExplode $
        processCitations $
          p0 & setMeta "reference-section-title" "参考文献"
            & setMeta "csl" style
  pure $
    Text.Pandoc.bottomUp removeTeXGomiStr $
      Text.Pandoc.bottomUp linkLocalCite p

refBlockToList :: Text.Pandoc.Block -> Text.Pandoc.Block
refBlockToList
  (Text.Pandoc.Div ("refs", ["references"], atts) divs) =
    Text.Pandoc.RawBlock "html" $
      LT.toStrict $
        renderHtml $
          applyAtts (map (both %~ T.unpack) atts) $
            H5.ul ! H5.id "refs" ! H5.class_ "references" $
              mapM_ listise divs
    where
      listise (Text.Pandoc.Div (ident, cls, map (both %~ T.unpack) -> ats) [Text.Pandoc.Para (Text.Pandoc.Str lab : dv)]) =
        applyAtts ats $
          H5.li ! H5.id (H5.textValue ident)
            ! H5.class_ (H5.textValue $ T.unwords $ "ref" : cls)
            ! H5.dataAttribute "ref-label" (H5.textValue $ unbracket lab)
            $ do
              H5.span ! H5.class_ "ref-label" $ H5.text lab
              H5.span ! H5.class_ "ref-body" $
                fromRight' $
                  Text.Pandoc.runPure $
                    Text.Pandoc.writeHtml5 writerConf $
                      Text.Pandoc.Pandoc Text.Pandoc.nullMeta [Text.Pandoc.Plain $ dropWhile (== Text.Pandoc.Space) dv]
      listise _ = ""
refBlockToList d = d

applyAtts :: Attributable b => [(String, String)] -> b -> b
applyAtts ats elt =
  let as = map (\(k, v) -> H5.customAttribute (fromString k) (fromString v)) ats
   in foldl (!) elt as

linkLocalCite :: Text.Pandoc.Inline -> Text.Pandoc.Inline
linkLocalCite (Text.Pandoc.Cite cs@(_ : _) bdy) =
  Text.Pandoc.Cite cs [Text.Pandoc.Link ("", [], []) bdy ("#ref-" <> Text.Pandoc.citationId (head cs), "")]
linkLocalCite i = i

remoteCiteLink :: T.Text -> HM.HashMap T.Text RefInfo -> Text.Pandoc.Inline -> Text.Pandoc.Inline
remoteCiteLink base refInfo (Text.Pandoc.Cite cs _) =
  let ctLinks =
        [ maybe
          (Text.Pandoc.Strong [Text.Pandoc.Str citationId])
          (\RefInfo {..} -> Text.Pandoc.Link ("", [], []) [Text.Pandoc.Str refLabel] (base <> "#" <> refAnchor, ""))
          mres
        | Text.Pandoc.Citation {..} <- cs
        , let mres = HM.lookup citationId refInfo
        ]
   in Text.Pandoc.Span ("", ["citation"], [("data-cites", T.intercalate "," $ map Text.Pandoc.citationId cs)]) $
        concat [[Text.Pandoc.Str "["], ctLinks, [Text.Pandoc.Str "]"]]
remoteCiteLink _ _ i = i

isReference :: Text.Pandoc.Block -> Bool
isReference (Text.Pandoc.Div (_, ["references"], _) _) = True
isReference _ = False

data RefInfo = RefInfo {refAnchor :: T.Text, refLabel :: T.Text}
  deriving (Read, Show, Eq, Ord)

buildRefInfo :: T.Text -> HM.HashMap T.Text RefInfo
buildRefInfo =
  foldMap go
    . filter (tagOpen (== "li") (maybe False (elem "ref" . T.words) . lookup "class"))
    . TS.parseTags
  where
    go ~(TagOpen _ atts) =
      maybe HM.empty (\(r, lab) -> HM.singleton r (RefInfo ("ref-" <> r) lab)) $
        (,) <$> (T.stripPrefix "ref-" =<< lookup "id" atts)
          <*> lookup "data-ref-label" atts

unbracket :: T.Text -> T.Text
unbracket (T.stripPrefix "[" -> Just l)
  | Just lab <- T.stripSuffix "]" l = lab
  | otherwise = l
unbracket lab = lab

removeTeXGomiStr :: Text.Pandoc.Inline -> Text.Pandoc.Inline
removeTeXGomiStr (Text.Pandoc.Str p) = Text.Pandoc.Str $ bakaReplace p
removeTeXGomiStr (Text.Pandoc.Emph is) = Text.Pandoc.Emph $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Strong is) = Text.Pandoc.Strong $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Strikeout is) = Text.Pandoc.Strikeout $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Superscript is) = Text.Pandoc.Superscript $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Subscript is) = Text.Pandoc.Subscript $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.SmallCaps is) = Text.Pandoc.SmallCaps $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Quoted ty is) = Text.Pandoc.Quoted ty $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Cite cs is) = Text.Pandoc.Cite cs $ map removeTeXGomiStr is
removeTeXGomiStr (Text.Pandoc.Math ty m) = Text.Pandoc.Math ty $ bakaReplace m
removeTeXGomiStr (Text.Pandoc.Link att is t) = Text.Pandoc.Link att (map removeTeXGomiStr is) t
removeTeXGomiStr (Text.Pandoc.Image att is t) = Text.Pandoc.Image att (map removeTeXGomiStr is) t
removeTeXGomiStr (Text.Pandoc.Note bs) = Text.Pandoc.Note $ Text.Pandoc.bottomUp removeTeXGomiStr bs
removeTeXGomiStr (Text.Pandoc.Span ats is) = Text.Pandoc.Span ats (map removeTeXGomiStr is)
removeTeXGomiStr i = i

bakaReplace :: T.Text -> T.Text
bakaReplace =
  T.replace "\\qed" ""
    . T.replace "\\mbox" ""
    . T.replace "~" ""
    . T.replace "\\printbibliography" ""
    . T.replace "\\printbibliography[title=参考文献]" ""
    . T.replace "\\\\printbibliography" "\\xxxxxxpbbl"
    . T.replace "\\RequirePackage{luatex85}" ""

unicodiseMath :: Text.Pandoc.Inline -> Text.Pandoc.Inline
unicodiseMath m@(Text.Pandoc.Math mode eqn) =
  let mmode
        | Text.Pandoc.InlineMath <- mode = UM.DisplayInline
        | otherwise = UM.DisplayBlock
      inls = either (const [m]) (fromMaybe [] . UM.writePandoc mmode) $ UM.readTeX eqn
   in Text.Pandoc.Span ("", ["math"], []) inls
unicodiseMath i = i

fromPure :: IsString a => Text.Pandoc.PandocPure a -> a
fromPure = fromRight "" . Text.Pandoc.runPure

myExts :: Text.Pandoc.Extensions
myExts = Text.Pandoc.disableExtension Text.Pandoc.Ext_latex_macros $ mconcat [Text.Pandoc.extensionsFromList exts, Text.Pandoc.pandocExtensions]
  where
    exts =
      [ Text.Pandoc.Ext_backtick_code_blocks
      , Text.Pandoc.Ext_definition_lists
      , Text.Pandoc.Ext_fenced_code_attributes
      , Text.Pandoc.Ext_footnotes
      , Text.Pandoc.Ext_raw_html
      , Text.Pandoc.Ext_raw_tex
      , Text.Pandoc.Ext_tex_math_dollars
      , Text.Pandoc.Ext_emoji
      ]

protocol :: T.Text -> Maybe T.Text
protocol url = T.pack . P.init . uriScheme <$> parseURI (T.unpack url)

linkCard :: Text.Pandoc.Pandoc -> Action Text.Pandoc.Pandoc
linkCard = Text.Pandoc.bottomUpM $ \case
  Text.Pandoc.Para bs | Just us <- checkCard bs, not (null us) -> toCards us
  Text.Pandoc.Plain bs | Just us <- checkCard bs, not (null us) -> toCards us
  b -> return b
  where
    toCards us = do
      tmpl <- itemBody <$> loadTemplate "templates/site-card.mustache"
      (gaths, protos, frams) <- unzip3 <$> mapM toCard us
      let gathered = and gaths && and (zipWith (==) protos (tail protos))
      return $
        Text.Pandoc.RawBlock "html" $
          LT.toStrict $
            Mus.renderMustache tmpl $
              object
                [ "gather" .= gathered
                , "frames" .= frams
                ]
    myStringify Text.Pandoc.Link {} = "LINK"
    myStringify l = stringify l
    checkCard = mapM isCard . filter (not . T.all isSpace . myStringify)
    isCard (Text.Pandoc.Link _ [] (url, ""))
      | not $ T.null url = Just url
    isCard _ = Nothing
    toCard origUrl = do
      Cards {..} <- readFromYamlFile' "config/cards.yml"
      let mproto = protocol origUrl
          Card {template = tmpl, gather} =
            fromMaybe defaultCard $ flip HM.lookup cardDic =<< mproto
          urlBody =
            fromMaybe origUrl $
              flip T.stripPrefix origUrl . (`T.snoc` ':') =<< mproto
          model =
            object
              [ "url" .= origUrl
              , "urlBody" .= urlBody
              , "gather" .= gather
              ]
          body = Mus.renderMustache tmpl model
      return (gather, fromMaybe "*" mproto, body)

data Log = Log
  { logLog :: T.Text
  , logTitle :: String
  , logDate :: T.Text
  , logIdent :: T.Text
  }
  deriving (Generic, Show)

toLog :: Item T.Text -> Action (Item Log)
toLog i = do
  let logIdent = T.pack $ takeBaseName $ itemPath i
      logLog =
        withTags (rewriteIDs logIdent) $
          demoteHeaders $ demoteHeaders $ itemBody i
      Just logTitle = lookupMetadata "title" i
  logDate <- itemDateStr i
  return $ Log {..} <$ i

rewriteIDs :: T.Text -> Tag T.Text -> Tag T.Text
rewriteIDs ident (TagOpen "a" atts)
  | Just rest <- T.stripPrefix "#" =<< lookup "href" atts
    , "fn" `T.isInfixOf` rest =
    TagOpen "a" $ ("href", mconcat ["#", ident, "-", rest]) : filter ((/= "href") . fst) atts
rewriteIDs ident (TagOpen t atts)
  | Just name <- lookup "id" atts
    , "fn" `T.isInfixOf` name =
    TagOpen t $ ("id", mconcat [ident, "-", name]) : filter ((/= "id") . fst) atts
rewriteIDs _ t = t

renderTalks :: MonadSake m => Maybe Int -> m (Item T.Text)
renderTalks mlen = do
  talkTplt <- loadItem "templates/talks.mustache"
  talks0 <- loadBinary @[Talk] (cacheD </> "talks.bin")
  let talks =
        map addItemInfo
          . maybe id take mlen
          . sortOn (Down . date)
          <$> talks0
      cxt =
        mconcat
          [ constField "talks" $ itemBody talks
          , constField "continued" $
              length (itemBody talks0) /= length (itemBody talks)
          ]
  applyAsMustache cxt talkTplt

logConf :: Options
logConf = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}

instance ToJSON Log where
  toJSON = genericToJSON logConf
