{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric           #-}
{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, GADTs, LambdaCase   #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell       #-}
{-# LANGUAGE TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-type-defaults         #-}
module Main where
import Breadcrumb
import Instances     ()
import Lenses
import Macro
import MathConv
import MissingSake
import Settings
import Utils
import Web.Sake.Feed

import           Blaze.ByteString.Builder (toByteString)
import           Control.Applicative      ((<|>))
import           Control.Concurrent       (newChan, readChan, threadDelay)
import           Control.Concurrent.Async (concurrently_)
import           Control.Lens             (iforM_, imap, ix, (%~), (.~), (^?),
                                           _2)
import           Control.Monad            hiding (mapM)
import           Control.Monad.State
import qualified Crypto.Hash.SHA256       as SHA
import           Data.Aeson               (Options, ToJSON (..), Value (..),
                                           camelTo2, defaultOptions,
                                           fieldLabelModifier, fromJSON,
                                           genericToJSON, toJSON)
import qualified Data.ByteString.Char8    as BS
import qualified Data.CaseInsensitive     as CI
import           Data.Char                hiding (Space)
import           Data.Data                (Data)
import           Data.Foldable            (asum)
import           Data.Function
import qualified Data.HashMap.Strict      as HM
import           Data.List
import qualified Data.List                as L
import qualified Data.List.Split          as L
import           Data.Maybe               (fromMaybe, isJust, isNothing,
                                           listToMaybe, mapMaybe, maybeToList)


import           Data.Monoid                     ((<>))
import           Data.Ord                        (comparing)
import qualified Data.Store                      as S
import           Data.String
import qualified Data.Text                       as T
import qualified Data.Text.ICU.Normalize         as UNF
import qualified Data.Text.Lazy                  as LT
import           Data.Text.Lens                  (packed, unpacked)
import           Data.Time
import           Data.Yaml                       (object, (.=))
import           GHC.Generics
import           Language.Haskell.TH             (litE, runIO, stringL)
import           Network.HTTP.Types
import           Network.URI
import           Network.Wai.Application.Static  (defaultWebAppSettings,
                                                  ssIndices, staticApp)
import           Network.Wai.Handler.Warp        (run)
import           Prelude                         hiding (mapM)
import qualified Prelude                         as P
import           Skylighting                     hiding (Context (..), Style)
import           System.Directory                (canonicalizePath,
                                                  createDirectoryIfMissing,
                                                  getCurrentDirectory,
                                                  getHomeDirectory,
                                                  getModificationTime,
                                                  renameFile)
import           System.Environment
import           System.FilePath.Posix
import           System.FSNotify                 (watchTreeChan, withManager)
import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5                ((!))
import qualified Text.Blaze.Html5                as H5
import qualified Text.Blaze.Html5.Attributes     as H5 hiding (span)
import           Text.Blaze.Internal             (Attributable)
import           Text.CSL                        (Reference, Style,
                                                  readBiblioFile, readCSLFile)
import           Text.CSL.Pandoc
import           Text.Hamlet
import           Text.HTML.TagSoup
import qualified Text.HTML.TagSoup               as TS
import           Text.HTML.TagSoup.Match
import           Text.LaTeX.Base                 (render)
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax          hiding ((<>))
import qualified Text.Mustache                   as Mus
import           Text.Pandoc                     hiding (getModificationTime,
                                                  runIO)
import           Text.Pandoc.Builder             hiding ((<>))
import qualified Text.Pandoc.Builder             as Pan
import           Text.Pandoc.Shared              (stringify, trim)
import qualified Text.TeXMath                    as UM
import           WaiAppStatic.Types              (unsafeToPiece)
import           Web.Sake                        hiding (Env)
import           Web.Sake.Conf
import           Web.Sake.Html

default (String, T.Text)

home :: FilePath
home = $(litE . stringL =<< runIO getHomeDirectory)

globalBib :: FilePath
globalBib = home </> "Library/texmf/bibtex/bib/myreference.bib"

myShakeOpts :: ShakeOptions
myShakeOpts = shakeOptions { shakeThreads = 0
                           , shakeProgress = progressSimple
                           , shakeChange = ChangeDigest
                           }

templateDir :: T.Text
templateDir = "templae"

siteConf :: SakeConf
destD :: FilePath
srcD :: FilePath
cacheD :: FilePath
siteConf@SakeConf{destinationDir = destD
                 ,sourceDir = srcD
                 ,cacheDir = cacheD
                 } =
  def{ destinationDir = "_site"
     , sourceDir = "site-src"
     , cacheDir = "_cache"
     , ignoreFile = \fp ->
         or [ "~" `L.isSuffixOf` fp
            , "#" `L.isSuffixOf` fp && "#" `L.isPrefixOf` fp
            , "//.DS_Store" ?== fp
            ]
     }

destToSrc :: FilePath -> FilePath
destToSrc = replaceDir (destinationDir siteConf) (sourceDir siteConf)

articleList :: FilePath
articleList = "_build" </> ".articles"

main :: IO ()
main = getArgs >>= \case
  "watch" : args -> watch args
  args -> withArgs args runShake

watch :: [String] -> IO ()
watch args = do
  chan <- newChan
  server `concurrently_` watcher chan `concurrently_` builder chan
  where
  server = run 8000 $ staticApp $ (defaultWebAppSettings destD) { ssIndices = [unsafeToPiece "index.html"]
                                                                }
  builder chan = do
    withArgs args runShake
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
copies = ["t//*" ,  "js//*" ,  ".well-known//*", "//*imgs//*", "//*img//*"
         ,"math/cellular-automaton//*", "math/computational-algebra-seminar//*"
         ,"prog/automaton//*", "prog/doc//*", "math//*.pdf", "prog/ruby//*"
         , "css//*.css", "css//*.map", "//*.key", "//*.webp"
         , "logs//*.jpg", "logs//*.png", "math/3d-mandelbrot/*"
         ,"katex//*", "keybase.txt", "math/cellular-automaton//*.png", "//*.jpg", "//*.gif", "//*.webm"
         ]

runShake :: IO ()
runShake = shakeArgs myShakeOpts $ do
  want ["site"]

  let routing = [ Convert ("//*.md" .||. "//*.html" .||. "//*.tex") (-<.> "html")
                , Convert "//*.tex" (-<.> "pdf")
                , Convert "//*.sass" (-<.> "css")
                , Convert "robots.txt" id
                , Cached  "//*.tex" (<.> "preprocess")
                ] ++ map Copy copies ++
                [ Create "feed.xml", Create "sitemap.xml", Create ".ignore" ]

  "build" ~> need ["site"]

  "deploy" ~> do
    ign <- T.lines <$> readTextFile' (destD </> ".ignore")
    () <- cmd_ "rsync" "--delete-excluded" "--checksum" "-av" (map (T.unpack . ("--exclude=" <>)) ign)
                 "_site/"
                 "sakura-vps:~/mighttpd/public_html/"
    () <- cmd_ "git" "add" "templates" "config" "site-src" "app"
    () <- cmd_ "git" "commit" "-amupdated"
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
        then cmd_ (EchoStdout False) (WithStdout True)
                  "sassc" "-tcompressed" (FileStdin origPath) (FileStdout out)
        else cmd_ (EchoStdout False) (WithStdout True)
                  "yuicompressor" origPath "-o" out

    (destD </> "robots.txt") %> \out -> do
      tmpl <- itemBody <$> loadItemWith out (srcD </> "robots.txt")
      drafts <- mapMaybe (stripDirectory destD . itemTarget) <$> listDrafts out
      let obj = object ["disallowed" .= map ('/':) drafts]
      writeLazyTextFile out $ Mus.renderMustache tmpl obj

    (destD </> "sitemap.xml") %> \out -> do
      items <- filter isPublished <$> listChildren True out
      let itemsCtx = urlField <> updatedDateField "date" "%Y-%m-%d" <> defaultContext
          ctx = mconcat [ defaultContext
                        , listField "items"
                            itemsCtx
                            items
                        ]
      writeTextFile out . itemBody =<< applyAsMustache ctx
        =<< loadItem "templates/sitemap.mustache"

    (destD </> "archive.html") %> \out -> do
      (count, items) <- renderPostList
                        =<< myRecentFirst
                        =<< listChildren False out
      let ctx = mconcat [ constField "child-count" count
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
        Item{..} <- loadItemWith out $ destToSrc $ out -<.> "tex"
        let opts = fromMaybe "-pdflua" $
                   maybeResult . fromJSON =<< HM.lookup "latexmk" itemMetadata
        withTempDir $ \tmp -> do
          writeTextFile (tmp </> texFileName) itemBody
          bibThere <- doesFileExist bibPath
          when bibThere $ copyFile' bibPath (tmp </> takeFileName bibPath)
          copyFile' ("data" </> ".latexmkrc") (tmp </> ".latexmkrc")
          cmd_ "latexmk" (EchoStdout False) (WithStdout True) (Cwd tmp) opts texFileName
          copyFileNoDep (tmp </> texFileName -<.> "pdf") out

    (destD </> "logs" </> "index.html") %> \out -> do
      logs <- mapM toLog
             =<< myRecentFirst
             =<< loadAllSnapshots siteConf
                 "content" { snapshotSource = "logs//*.md" .&&. complement "logs/index.md"
                           }
      let pages = L.chunksOf 5 logs
          toName 0 = out
          toName n = dropExtension out ++ "-" ++ show n <.> "html"
      tmpl <- myPandocCompiler out
      makePagination mempty tmpl toName pages

    (destD </> "index.html") %> \out -> do
      (count, posts) <- renderPostList . take 5
                        =<< aggregateLogs
                        =<< myRecentFirst
                        =<< listChildren False out
      let ctx = mconcat [ constField "child-count" (show count)
                        , constField "updates" posts
                        , myDefaultContext
                        ]
      let mdPath = destToSrc $ out -<.> "md"
      mdThere <- doesFileExist mdPath
      let pth | mdThere = mdPath
              | otherwise = mdPath -<.> "html"
      loadItemWith out pth
        >>= compilePandoc readerConf writerConf
        >>= applyDefaultTemplate ctx {- tags -}
        >>= writeTextFile out . itemBody

    fromString (destD <//> "index.html") .&&. complement (disjoin copies) %%> \out -> do
      (count, chl) <- renderPostList
                      =<< myRecentFirst
                      =<< listChildren False out
      let ctx = mconcat [ constField "child-count" (show count)
                        , constField "children" chl
                        , myDefaultContext
                        ]
      writeTextFile out . itemBody
        =<< applyDefaultTemplate ctx
        =<< myPandocCompiler out

    fromString (destD <//> "*.html") .&&. complement (disjoin copies) %%> \out -> do
      srcPath <- getSourcePath siteConf out
      need [srcPath]
      if | ".tex"  `L.isSuffixOf` srcPath -> texToHtml  out
         | ".md"   `L.isSuffixOf` srcPath -> mdOrHtmlToHtml out
         | ".html" `L.isSuffixOf` srcPath ->
           if (destD </> "articles") `L.isPrefixOf` out
           then mdOrHtmlToHtml out
           else copyFile' srcPath out

    (cacheD <//> "*.tex.preprocess") %> \out -> do
      macs <- readFromYamlFile' "config/macros.yml"
      let origPath = replaceDir cacheD srcD $ dropExtension out
      i@Item{..} <- loadItemWith out origPath
      let cmacs = itemMacros i
          ans   = preprocessLaTeX (cmacs <> macs) itemBody
          go    = do
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
      loadAllSnapshots siteConf "content" { snapshotSource = ("//*.md" .||. "//*.html" .||. "//*.tex") .&&. complement ("draft//*" .||. "logs/index*.md" .||. "logs/index*.html")
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
  l0  <- L.stripPrefix "image-" fname
  readMaybe $ dropExtension l0

texToHtml :: FilePath -> Action ()
texToHtml out = do
  i0 <- loadItemWith @_ @T.Text out $ destToSrc $ out -<.> "tex"
  PreprocessedLaTeX{..} <- readFromBinaryFile' (replaceDir srcD cacheD (itemPath i0) <.> "preprocess")
  let imgs = maybe [] (\(j, _) -> concat
                                  [ [base <.> "png", base <.> "svg"]
                                  | i <- [0..j-1]
                                  , let base = dropExtension out  </> "image-" ++ show i
                                  ])
             images
  putNormal $ "requiring images: "  ++ show imgs
  needed $ [ destD </> "katex" </> "katex.min.js" , out -<.> "pdf" ]
        ++ imgs
  (style, bibs) <- cslAndBib out
  ipan <-  linkCard . addPDFLink ("/" </> dropDirectory1 out -<.> "pdf")
         . addAmazonAssociateLink "konn06-22"
       =<< procSchemes . myProcCites style bibs
       =<< liftIO (texToMarkdown out latexSource)
  let html = "{{=<% %>=}}\n" <> fromPure (writeHtml5String writerConf ipan)
      panCtx = pandocContext $
               ipan & _Pandoc . _2 %~ (RawBlock "html" "{{=<% %>=}}\n":)
  writeTextFile out . itemBody
    =<< applyDefaultTemplate panCtx . fmap ("{{=<% %>=}}\n" <>)
    =<< applyAsMustache panCtx (setItemBody html i0)

mdOrHtmlToHtml :: FilePath -> Action ()
mdOrHtmlToHtml out =
  myPandocCompiler out
  >>= applyDefaultTemplate myDefaultContext
  >>= writeTextFile out . itemBody

cslAndBib :: FilePath -> Action (Style, [Reference])
cslAndBib fp = do
  let cslPath = srcD </> fp -<.> "csl"
      bibPath = srcD </> fp -<.> "bib"
  mbib <- tryWithFile bibPath $ readFromFileNoDep bibPath
  gbib <- liftIO $ readBiblioFile (const True) globalBib
  customCSL <- doesFileExist cslPath
  style <-
    if customCSL
    then needing (liftIO . readCSLFile Nothing) cslPath
    else needing (liftIO . readCSLFile Nothing) ("data" </> "default.csl")
  let bibs = fromMaybe [] mbib ++ gbib
  return (style, bibs)

pandocContext :: Pandoc -> Context a
pandocContext (Pandoc meta _)
  | Just abst <- lookupMeta "abstract" meta =
        constField "abstract" $ T.unpack $
        fromPure $
        writeHtml5String writerConf $ Pandoc meta (mvToBlocks abst)
  | otherwise = mempty

listDrafts :: FilePath -> Action [Item T.Text]
listDrafts fp =
  filter (not . isPublished) <$> listChildren False fp

addPDFLink :: FilePath -> Pandoc -> Pandoc
addPDFLink plink (Pandoc meta body) = Pandoc meta body'
  where
    Pandoc _ body' = doc $ mconcat [ para $ mconcat [ "[", link' plink "PDF版" "PDF版", "]"]
                                   , Pan.fromList body
                                   ]

link' :: (H5.ToMarkup a2, H5.ToMarkup a1) => a1 -> a2 -> p -> Inlines
link' l txt _ttl =
  rawInline "html" $ renderHtml
    [shamlet|<a href=#{l} onclick="ga('send', 'event', 'download', 'ac', '#{l}')">#{txt}|]

listChildren :: Bool -> FilePath -> Action [Item T.Text]
listChildren includeIndices out = do
  let dir = takeDirectory $ destToSrc  out
      excludes = disjoin $
                 fromString out :
                 concat [["//index.md", "archive.md", "archive.md", "//index.md"] | not includeIndices] ++ copies
  mapM loadItemToHtml . filter (not . (?===) excludes . dropDirectory1) . map  (dir </>)
    =<< globDirectoryFiles dir subContentsWithoutIndex

loadItemToHtml :: (Readable a, MonadSake f) => FilePath -> f (Item a)
loadItemToHtml fp = loadItemWith (replaceDir srcD destD fp -<.> "html") fp

subContentsWithoutIndex :: Patterns
subContentsWithoutIndex =
  ("*//*.html" .||. "//*.md" .||. "//*.tex")
  .&&. complement (disjoin [ "prog/doc//*", "prog/ruby//*", "prog/automaton//*"
                           , "math/cellular-automaton//*"
                           , "t//*"
                           , "math/computational-algebra-seminar//*"
                           ])

feedCxt :: Context T.Text
feedCxt =  mconcat [ field "published" itemDateFeedStr
                   , urlField
                   , field "updated" itemDateFeedStr
                   , bodyField "description"
                   , defaultContext
                   ]

normaliseFeed :: T.Text -> T.Text
normaliseFeed = concatMapTags $ \case
  TagOpen "iframe" _ -> []
  TagClose "iframe" -> []
  TagOpen op atts  -> [TagOpen op $ filter (not . T.isPrefixOf "data-" . fst) atts]
  t -> [t]

itemDateFeedStr :: MonadSake m => Item a -> m T.Text
itemDateFeedStr = fmap (insertColon . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z") . itemDate
  where
    insertColon txt =
      let (bh, ah) = T.splitAt (T.length txt - 2) txt
      in bh <> ":" <> ah

itemDateStr :: MonadSake m => Item a -> m T.Text
itemDateStr = fmap (T.pack . formatTime defaultTimeLocale "%Y/%m/%d %X %Z") . itemDate

feedConf :: FeedConf
feedConf =
  FeedConf { feedTitle       = "konn-san.com 建設予定地"
           , feedDescription = "数理論理学を中心に数学、Haskell、推理小説、評論など。"
           , feedAuthor      = FeedAuthor { authorName = "Hiromi ISHII", authorEmail = "konn.jinro_at_gmail.com" }
           , feedRoot        = "https://konn-san.com"
           , feedUrl         = "/feed.xml"
           }


writerConf :: WriterOptions
writerConf =
  def{ writerHTMLMathMethod = MathJax "https://konn-san.com/math/mathjax/MathJax.js?config=xypic"
     , writerHighlightStyle = Just pygments
     , writerSectionDivs = True
     , writerExtensions = disableExtension Ext_tex_math_dollars myExts
     }

readerConf :: ReaderOptions
readerConf = def { readerExtensions = myExts }

myPandocCompiler :: FilePath -> Action (Item T.Text)
myPandocCompiler out = do
  (csl, bib) <- cslAndBib out
  loadOriginal siteConf out
    >>= readPandoc readerConf
    >>= mapM (procSchemes . myProcCites csl bib
              <=< linkCard . addAmazonAssociateLink "konn06-22")
    >>= writePandoc writerConf

resolveRelatives :: FilePath -> FilePath -> FilePath
resolveRelatives rt pth =
  let revRoots = reverse $ splitPath rt
  in go revRoots $ splitPath pth
  where
    go _        ("/" : rest)   = go [] rest
    go (_ : rs) ("../" : rest) = go rs rest
    go []       ("../" : rest) = go [".."] rest
    go r        ("./" : rest)  = go r rest
    go rs       (fp  : rest)   = go (fp : rs) rest
    go fps      []             = joinPath $ reverse fps

applyDefaultTemplate :: Context T.Text -> Item T.Text -> Action (Item T.Text)
applyDefaultTemplate addCtx item@Item{..} = do
  nav <- makeNavBar itemIdentifier
  let r     = makeRelative destD $ itemPath item
      imgs   = map (("https://konn-san.com/" <>) . resolveRelatives (takeDirectory r) . T.unpack) $
               extractLocalImages $ TS.parseTags itemBody
      navbar = constField "navbar" nav
      thumb  = constField "thumbnail" $
               fromMaybe "https://konn-san.com/img/myface_mosaic.jpg" $
               listToMaybe imgs
      unpublished = field "unpublished" $ return . not . isPublished
      date = field "date" itemDateStr
      -- toc = field "toc" $ return . buildTOC . readHtml' readerConf . T.pack . itemBody
      cxt  = mconcat [ thumb, shortDescrCtx, unpublished, addCtx, navbar
                     , breadcrumbContext "breadcrumb"
                     , headElemsCtx, metaTags, date, noTopStar, myDefaultContext
                     ]
  let item' = demoteHeaders . withTags addRequiredClasses <$> item
  scms <- readFromYamlFile' "config/schemes.yml"
  i'' <-  mapM (procKaTeX . relativizeUrlsTo (dropDirectory1 itemTarget))
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
    cmd_ (Cwd tmp) "tex2img" (EchoStdout False) (WithStdout True)
      ["--latex=luajittex --fmt=luajitlatex.fmt"]
      "--with-text" "image.tex" "image.svg"
    -- Generating PNGs
    cmd_ (Cwd tmp) (EchoStdout False) (WithStdout True)
          "convert" "-density" "200" "image.pdf" "image-%d.png"
    Stdout infos <-
      cmd (Cwd tmp) "pdftk"
          (EchoStdout False) (WithStdout True)
          "image.pdf" "dump_data_utf8"
    let pages = fromMaybe (0 :: Integer) $ listToMaybe $ mapMaybe
                 (readMaybe <=< L.stripPrefix "NumberOfPages: ")  (lines infos)
    forM_ [1..pages - 1] $ \n -> do
      let targ = tmp </> "image-" <> show n <.> "svg"
      liftIO $ renameFile (tmp </> "image-" <> show (n + 1) <.> "svg") targ
    liftIO $ renameFile (tmp </> "image.svg") (tmp </> "image-0.svg")
    imgs <- getDirectoryFiles  tmp ["*.png", "*.svg"]
    forM_ imgs $ \i ->
      traced (unwords ["Copying", tmp </> i, "to", master </> i])  $
        copyFileNoDep (tmp </> i) (master </> i)
    putNormal . unwords . ("generated:" :) =<< getDirectoryFiles tmp ["*.png", "*.svg"]


relativizeUrlsTo :: FilePath -> T.Text -> T.Text
relativizeUrlsTo targ = withUrls $ unpacked %~ makeRelative targ

headElemsCtx :: Context a
headElemsCtx = field_ "head" $ \i ->
  let fp = runIdentifier $ itemIdentifier i
      m | "//math//*" ?== fp = renderHtml [shamlet|<link rel="stylesheet" href="/css/math.css">|]
           | otherwise = ""
      h = fromMaybe "" $ lookupMetadata "head" i
  in unlines [m, h]

descriptionField :: String -> Context a
descriptionField key =
  field_ key $ fromMaybe "" . lookupMetadata "description"

shortDescrCtx :: Context a
shortDescrCtx = field_ "short_description" $ \i ->
  let descr = fromMaybe "" $ lookupMetadata "description" i
  in either (const "") (T.unpack . T.replace "\n" " ") $ runPure $
     writePlain def . bottomUp unicodiseMath =<< readMarkdown readerConf descr

noTopStar :: Context b
noTopStar = field_ "no-top-star" $ \i ->
  case lookupMetadata "top-star" i of
    Just False -> True
    _          -> False

metaTags :: Context a
metaTags = field "meta" $ \i -> do
   let [desc0, tags] = flip map ["description", "tag"] $ \k ->
         fromMaybe "" $ lookupMetadata k i
   let desc = fromPure $
              writePlain writerConf { writerHTMLMathMethod = PlainMath
                                    , writerWrapText = WrapNone }
              =<< readMarkdown readerConf (T.pack desc0)
   return $ renderHtml $ do
     H5.meta ! H5.name "Keywords"    ! H5.content (H5.toValue tags)
     H5.meta ! H5.name "description" ! H5.content (H5.toValue desc)


useKaTeX :: Item a -> Bool
useKaTeX item =
  fromMaybe True $ lookupMetadata "katex" item

procKaTeX :: MonadAction m => T.Text -> m T.Text
procKaTeX = liftAction . fmap T.pack . prerenderKaTeX . T.unpack

prerenderKaTeX :: String -> Action String
prerenderKaTeX src = do
  nodePath <- getEnvWithDefault "" "NODE_PATH"
  wd <- liftIO getCurrentDirectory
  let katexD = wd </> srcD </> "katex"
  let paths = L.intercalate ":" $ [katexD </> "contrib", katexD] ++ L.splitOn ":" nodePath
  putNormal $ "NODE_PATH=" <> paths
  Stdout out <-
    cmd (Cwd "data") "node"
        (EchoStdout False) (WithStdout True)
        (AddEnv "NODE_PATH" paths)
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
addRequiredClasses (TagOpen "table" attr) = TagOpen "table" (("class", "table"):attr)
addRequiredClasses (TagOpen "blockquote" attr) = TagOpen "blockquote" (("class", "blockquote"):attr)
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


procSchemes :: Pandoc -> Action Pandoc
procSchemes = bottomUpM procSchemes0

procSchemesUrl :: Schemes -> T.Text -> T.Text
procSchemesUrl (Schemes dic) =
  withUrls $ \u ->
  case parseURI $ T.unpack u of
    Just URI{..}
      | Just Scheme{..} <- HM.lookup (T.init $ T.pack uriScheme) dic
      -> let body = mconcat [ maybe "" uriAuthToString uriAuthority
                            , uriPath
                            , uriQuery
                            , uriFragment
                            ]
         in prefix <> T.pack body <> fromMaybe "" postfix
    _  -> u

uriAuthToString :: URIAuth -> String
uriAuthToString (URIAuth a b c) = concat [a, b, c]

procSchemes0 :: Inline -> Action Inline
procSchemes0 inl =
  case inl ^? linkUrl of
    Nothing -> return inl
    Just url -> do
      Schemes dic <- readFromYamlFile' "config/schemes.yml"
      let url' = maybe url T.unpack $ asum $
                 imap (\k v -> fmap (sandwitched (prefix v) (fromMaybe "" $ postfix v)) $
                               T.stripPrefix (k <> ":") $ T.pack url)
                 dic
      return $ inl & linkUrl .~ url'
  where
    sandwitched s e t = s <> t <> e

addAmazonAssociateLink :: String -> Pandoc -> Pandoc
addAmazonAssociateLink = bottomUp . procAmazon

addAmazonAssociateLink' :: String -> T.Text -> T.Text
addAmazonAssociateLink' tag = withUrls (unpacked %~ attachTo tag)

procAmazon :: String -> Inline -> Inline
procAmazon tag (Link atts is (url, ttl))  = Link atts is (attachTo tag url, ttl)
procAmazon tag (Image atts is (url, ttl)) = Image atts is (attachTo tag url, ttl)
procAmazon _   il                      = il

attachTo :: String -> String -> String
attachTo key url
    | (p@("http:":"":amazon:paths), qs) <- decodePath (BS.pack url)
    , amazon `elem` amazons
    , let cipath = map CI.mk paths
    , ["o", "asin"] `isPrefixOf` cipath || "dp" `elem` cipath
                        || ["gp", "product"] `isPrefixOf` cipath
    , isNothing (lookup "tag" qs)
         = tail $ BS.unpack $ toByteString $
           encodePath p (("tag", Just $ BS.pack key):qs)
attachTo _   url = url

amazons :: [T.Text]
amazons = "www.amazon.com":"amazon.com":concatMap (\cc -> [T.concat [www,"amazon.",co,cc] | www <- ["","www."], co <- ["co.", ""]]) ccTLDs

ccTLDs :: [T.Text]
ccTLDs = ["jp"]

getActive :: [(T.Text, String)] -> FilePath -> String
getActive _ "archive.md" = "/archive.html"
getActive _ "profile.md" = "/profile.html"
getActive cDic ident = fromMaybe "/" $ listToMaybe $ filter p $ map snd cDic
  where
    p "/"       = False
    p ('/':inp) = fromString (inp++"//*") ?== ident
    p _         = False

makeNavBar :: Identifier -> Action String
makeNavBar ident = do
  NavBar cDic <- readFromYamlFile' "config/navbar.yml"
  let cats = toJSON [object ["path" .= pth
                            ,"category" .= cat
                            ,"active" .= (getActive cDic (dropDirectory1 $ runIdentifier ident) == pth)
                            ]
                    | (cat, pth) <- cDic
                    ]
  src <- readFromFile' "templates/navbar.mustache"
  return $ LT.unpack $ Mus.renderMustache src cats

data Pagination = Pagination { paginationPrev  :: Maybe Page
                             , paginationNext  :: Maybe Page
                             , paginationPages :: [Page]
                             }
  deriving (Read, Show, Eq, Ord, Generic)

paginationConf :: Options
paginationConf = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

data Page = Page { pageLink   :: String
                 , pageNumber :: Int
                 , pageActive :: Bool
                 }
  deriving (Read, Show, Eq, Ord, Generic)

pageConf :: Options
pageConf = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToJSON Page where
  toJSON = genericToJSON pageConf

instance ToJSON Pagination where
  toJSON = genericToJSON paginationConf

makePagination :: (ToJSON [a])
               => Context T.Text
               -> Item T.Text        -- ^ page template
               -> (Int -> FilePath)  -- ^ File name generator
               -> [[Item a]]         -- ^ items to be paginated (each list is accesible in field "items")
               -> Action ()
makePagination ctx0 page toName iss = do
  tmpl <- readFromFile' "templates/pagination.mustache"
  let ps = map (\i -> Page (replaceDir destD "/" $ toName i) i False) [0..length iss-1]
      pag i = Pagination { paginationPages = ps & ix i %~ \p -> p { pageActive = True }
                         , paginationPrev  =
                             if i > 0
                             then Just (ps !! (i - 1))
                             else Nothing
                         , paginationNext =
                             if i < length ps-1
                             then Just (ps !! (i + 1))
                             else Nothing
                         }
  iforM_ iss $ \i is -> do
    let pagNav = Mus.renderMustache tmpl (toJSON $ pag i)
        ctx = mconcat [ constField "items" (map itemBody is)
                      , constField "pagination" pagNav
                      , ctx0
                      , myDefaultContext
                      ]
    writeTextFile (toName i) . itemBody
      =<< applyDefaultTemplate ctx page { itemTarget = toName i }

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
          anc   = '#' : takeBaseName targ
          lastUpdate = T.concat ["（最終更新：", T.pack $ takeBaseName targ, "）"]
          meta' = itemMetadata i
                & HM.insertWith (flip appendText) "description" (String lastUpdate)
                & HM.insert "date" (fromMaybe (String "") $ HM.lookup "date" (itemMetadata a))
          i' = i { itemTarget = destD </> "logs/index.html" ++ anc
                 , itemMetadata = meta'
                 }
      return $ ps ++ i' : rest
    (ps, qs)     -> return $ ps ++ qs

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
      pdfField  = field "pdf" $ \Item{..} ->
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
               writeHtml5String writerConf . bottomUp (remoteCiteLink fp refs)
               =<< readMarkdown readerConf descr

      iCtxs = mconcat [ pdfField,  myDateField, urlField
                      , descField, defaultContext
                      ]
  src <- procKaTeX =<< applyTemplateList postItemTpl iCtxs posts
  return (length posts, src)

myDefaultContext :: Context T.Text
myDefaultContext =
  mconcat [ disqusCtx, defaultContext, canonicContext ]
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
  return $ map snd $ sortBy (flip $ comparing (zonedTimeToLocalTime . fst)) $ zip ds is

isPublished :: Item a -> Bool
isPublished item =
  let pub   = lookupMetadata "published" item
      dra   = lookupMetadata "draft"     item
  in fromMaybe True $ pub <|> (not <$> dra)

itemMacros :: Item a -> TeXMacros
itemMacros Item{..} =
  fromMaybe HM.empty $
  maybeResult . fromJSON =<< HM.lookup "macros" itemMetadata

capitalise :: String -> String
capitalise ""      = ""
capitalise (c: cs) = toUpper c : map toLower cs

itemDate :: MonadSake m => Item a -> m ZonedTime
itemDate item =
  let ident = itemIdentifier item
      mdate = parseTimeM True defaultTimeLocale "%Y/%m/%d %X %Z"
              =<< lookupMetadata "date" item
  in case mdate of
    Just date -> return date
    Nothing ->
      liftIO $
      utcToLocalZonedTime
      =<< getModificationTime (runIdentifier ident)

extractCites :: Data a => a -> [[Citation]]
extractCites = queryWith collect
  where
    collect (Cite t _) = [t]
    collect _          = []


extractNoCites :: Data c => c -> [[Citation]]
extractNoCites = queryWith collect
  where
    collect (RawInline "latex" src) =
      case parseLaTeX $ T.pack src of
        Left _ -> []
        Right t -> flip queryWith t $ \case
          TeXComm "nocite" [cs] -> [[ Citation (trim $ T.unpack w) [] [] NormalCitation 0 0
                                   | w <- T.splitOn "," $ T.init $ T.tail $ render cs]]
          _ -> []
    collect _ = []

myProcCites :: Style -> [Reference] -> Pandoc ->  Pandoc
myProcCites style bib p =
  let cs = extractCites p
      pars  = map (Para . pure . flip Cite []) $ cs ++ extractNoCites p
      -- Pandoc _ bibs = processCites style bib (Pandoc mempty pars)
      Pandoc info pan' = processCites style bib p
      refs = bottomUp refBlockToList $ filter isReference pan'
      body = filter (not . isReference) pan'
  in bottomUp removeTeXGomiStr $ bottomUp linkLocalCite $
     if null pars
     then p
     else Pandoc info (body ++ [Header 1 ("biblio", [], []) [Str "参考文献"]] ++ refs)

refBlockToList :: Block -> Block
refBlockToList
  (Div ("refs", ["references"], atts) divs) =
    RawBlock "html" $ renderHtml $
    applyAtts atts $
    H5.ul ! H5.id "refs" ! H5.class_ "references" $
    mapM_ listise divs
  where
    listise (Div (ident, cls, ats) [Para (Str lab : dv)]) =
      applyAtts ats $
      H5.li ! H5.id (H5.stringValue ident)
            ! H5.class_ (H5.stringValue $ unwords $ "ref" : cls)
            ! H5.dataAttribute "ref-label" (fromString $ unbracket lab) $ do
              H5.span ! H5.class_ "ref-label" $
                fromString lab
              H5.span ! H5.class_ "ref-body" $
                fromRight $ runPure $
                writeHtml5 writerConf $
                Pandoc nullMeta [Plain $ dropWhile (== Space) dv]
    listise _ = ""
refBlockToList d = d

applyAtts :: Attributable b => [(String, String)] -> b -> b
applyAtts ats elt =
  let as = map (\(k, v) -> H5.customAttribute (fromString k) (fromString v)) ats
  in foldl (!) elt as

linkLocalCite :: Inline -> Inline
linkLocalCite (Cite cs bdy) =
  Cite cs [Link ("", [], []) bdy ("#ref-" ++ citationId (head cs), "")]
linkLocalCite i = i

remoteCiteLink :: String -> HM.HashMap T.Text RefInfo -> Inline -> Inline
remoteCiteLink base refInfo (Cite cs _) =
  let ctLinks = [ maybe
                    (Strong [Str citationId])
                    (\RefInfo{..} -> Link ("", [], []) [Str $ T.unpack refLabel] (base <> "#" <> T.unpack refAnchor, ""))
                    mres
                | Citation{..} <- cs
                , let mres = HM.lookup (T.pack citationId) refInfo
                ]
  in Span ("", ["citation"], [("data-cites", intercalate "," $ map citationId cs)]) $
     concat [ [Str "["], ctLinks, [Str "]"]]
remoteCiteLink _ _ i                    = i

isReference :: Block -> Bool
isReference (Div (_, ["references"], _) _) = True
isReference _                              = False

data RefInfo = RefInfo { refAnchor :: T.Text, refLabel :: T.Text }
             deriving (Read, Show, Eq, Ord)

buildRefInfo :: T.Text -> HM.HashMap T.Text RefInfo
buildRefInfo =
  foldMap go
  .
  filter (tagOpen (== "li") (maybe False (elem "ref" . T.words) .  lookup "class"))
  .
  TS.parseTags
  where
    go ~(TagOpen _ atts) =
      maybe HM.empty (\(r, lab) -> HM.singleton r (RefInfo ("ref-" <> r) lab)) $
        (,) <$> (T.stripPrefix "ref-" =<< lookup "id" atts)
            <*> lookup "data-ref-label" atts

unbracket :: String -> String
unbracket ('[':l)
  | Just lab <- T.stripSuffix "]" (T.pack l) = T.unpack lab
  | otherwise = l
unbracket lab = lab

removeTeXGomiStr :: String -> String
removeTeXGomiStr = packed %~ T.replace "\\qed" ""
                           . T.replace "\\mbox" ""
                           . T.replace "~" ""
                           . T.replace "\\xxxxxxpbbl" "\\printbibliography"
                           . T.replace "\\printbibliography" ""
                           . T.replace "\\printbibliography[title=参考文献]" ""
                           . T.replace "\\\\printbibliography" "\\xxxxxxpbbl"
                           . T.replace "\\RequirePackage{luatex85}" ""

unicodiseMath :: Inline -> Inline
unicodiseMath m@(Math mode eqn) =
  let mmode | InlineMath <- mode = UM.DisplayInline
            | otherwise = UM.DisplayBlock
      inls = either (const [m]) (fromMaybe [] . UM.writePandoc mmode) $ UM.readTeX eqn
  in Span ("", ["math"], []) inls
unicodiseMath i = i

fromPure :: IsString a => PandocPure a -> a
fromPure = either (const "") id . runPure

myExts :: Extensions
myExts = mconcat [extensionsFromList exts, pandocExtensions]
  where
    exts = [ Ext_backtick_code_blocks
           , Ext_definition_lists
           , Ext_fenced_code_attributes
           , Ext_footnotes
           , Ext_raw_html
           , Ext_raw_tex
           , Ext_tex_math_dollars
           , Ext_emoji
           ]

protocol :: T.Text -> Maybe T.Text
protocol url = T.pack . P.init . uriScheme <$> parseURI (T.unpack url)

linkCard :: Pandoc -> Action Pandoc
linkCard = bottomUpM $ \case
  Para  bs | Just us <- checkCard bs, not (null us) -> toCards us
  Plain bs | Just us <- checkCard bs, not (null us) -> toCards us
  b -> return b
  where
    toCards us = do
      tmpl <- itemBody <$> loadTemplate "templates/site-card.mustache"
      (gaths, protos, frams) <- unzip3 <$> mapM toCard us
      let gathered = and gaths && and (zipWith (==) protos (tail protos))
      return $ RawBlock "html" $
        LT.unpack $
        Mus.renderMustache tmpl $
          object [ "gather" .= gathered
                 , "frames" .= frams
                 ]
    myStringify Link{} = "LINK"
    myStringify l      = stringify l
    checkCard = mapM isCard . filter (not . all isSpace . myStringify)
    isCard (Link _ [] (url, "")) = Just url
    isCard _                     = Nothing
    toCard (T.pack -> origUrl) = do
      Cards{..} <- readFromYamlFile' "config/cards.yml"
      let mproto = protocol origUrl
          Card{template = tmpl,gather} =
            fromMaybe defaultCard $ flip HM.lookup cardDic =<< mproto
          urlBody = fromMaybe origUrl $
                    flip T.stripPrefix origUrl . (`T.snoc` ':') =<< mproto
          model = object [ "url" .= origUrl
                         , "urlBody" .= urlBody
                         , "gather" .= gather
                         ]
          body = Mus.renderMustache tmpl model
      return (gather, fromMaybe "*" mproto, body)

data Log = Log { logLog   :: T.Text
               , logTitle :: String
               , logDate  :: T.Text
               , logIdent :: T.Text
               }
  deriving (Generic, Show)

toLog :: Item T.Text -> Action (Item Log)
toLog i = do
  let logIdent = T.pack $ takeBaseName $ itemPath i
      logLog = withTags (rewriteIDs logIdent) $
               demoteHeaders $ demoteHeaders $ itemBody i
      Just logTitle = lookupMetadata "title" i
  logDate <- itemDateStr i
  return $ const Log{..} <$> i

rewriteIDs :: T.Text -> Tag T.Text -> Tag T.Text
rewriteIDs ident (TagOpen "a" atts)
  | Just rest <- T.stripPrefix "#" =<< lookup "href" atts
  , "fn" `T.isInfixOf` rest =
      TagOpen "a" $ ("href", mconcat ["#", ident, "-", rest]) : filter ((/="href") . fst) atts
rewriteIDs ident (TagOpen t atts)
  | Just name <- lookup "id" atts, "fn" `T.isInfixOf` name  =
      TagOpen t $ ("id", mconcat [ident,"-",name]) : filter ((/="id") . fst) atts
rewriteIDs _ t = t

logConf :: Options
logConf = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance ToJSON Log where
  toJSON = genericToJSON logConf
