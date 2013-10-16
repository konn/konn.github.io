{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE PatternGuards, QuasiQuotes, TemplateHaskell, TupleSections  #-}
{-# LANGUAGE ViewPatterns                                                #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-type-defaults         #-}
module Main where
import           Blaze.ByteString.Builder        (toByteString)
import           Control.Applicative
import           Control.Monad                   hiding (mapM, sequence)
import qualified Data.ByteString.Char8           as BS
import qualified Data.CaseInsensitive            as CI
import           Data.List                       hiding (span, stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import qualified Data.Text                       as T
import           Data.Time
import           Data.Traversable                hiding (forM)
import           Filesystem
import           Filesystem.Path.CurrentOS       hiding (concat, null, (<.>),
                                                  (</>))
import qualified Filesystem.Path.CurrentOS       as Path
import           Hakyll                          hiding (fromFilePath,
                                                  toFilePath)
import qualified Hakyll
import           Instances
import           MathConv
import           Network.HTTP.Types
import           Prelude                         hiding (FilePath, div, mapM,
                                                  sequence, span)
import           Shelly
import           System.Locale
import           Text.Blaze.Html.Renderer.String
import           Text.CSL                        (readCSLFile)
import           Text.CSL.Pandoc
import           Text.Hamlet
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Pandoc
import           Text.Pandoc.Builder             hiding (fromList)
import qualified Text.Pandoc.Builder             as Pan

default (T.Text)

toFilePath :: Identifier -> FilePath
toFilePath = decodeString . Hakyll.toFilePath

fromFilePath :: FilePath -> Identifier
fromFilePath = Hakyll.fromFilePath . encodeString

main :: IO ()
main = hakyllWith config $ do
  match "*.css" $ route idRoute >> compile compressCssCompiler

  match ("js/**" .||. "robots.txt" .||. "img/**" .||. "favicon.ico" .||. "files/**") $
    route idRoute >> compile copyFileCompiler

  match "css/**" $
    route idRoute >> compile compressCssCompiler

  match "templates/**" $ compile templateCompiler

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      posts <- postList (Just 5) $ subContentsWithoutIndex
      myPandocCompiler
              >>= applyAsTemplate (constField "updates" posts <> defaultContext)
              >>= applyDefaultTemplate >>= relativizeUrls

  match "archive.md" $ do
    route $ setExtension "html"
    compile $ do
      posts <- postList Nothing $ subContentsWithoutIndex
      myPandocCompiler
              >>= applyAsTemplate (constField "children" posts <> defaultContext)
              >>= applyDefaultTemplate >>= relativizeUrls

  match ("*/index.md") $ do
    route $ setExtension "html"
    compile $ do
      chs <- listChildren True
      children <- postList Nothing (fromList $ map itemIdentifier chs)
      myPandocCompiler >>= applyAsTemplate (constField "children" children <> defaultContext)
                       >>= applyDefaultTemplate >>= saveSnapshot "content"  >>= relativizeUrls

  match "t/**" $ route idRoute >> compile copyFileCompiler

  match "prog/automaton/**" $ route idRoute >> compile copyFileCompiler

  match ("math/**.pdf") $ route idRoute >> compile copyFileCompiler
  match ("**.key") $ route idRoute >> compile copyFileCompiler

  match ("prog/doc/*/**") $
    route idRoute >> compile copyFileCompiler
  match ("**.html" .&&. complement ("prog/doc/**.html" .||. "templates/**")) $
    route idRoute >> compile copyFileCompiler
  match ("**.csl") $ compile cslCompiler
  match ("**.bib") $ compile (fmap biblioToBibTeX <$> biblioCompiler)
  match ("math/**.tex") $ version "html" $ do
    route $ setExtension "html"
    compile $ do
      fp <- decodeString . fromJust <$> (getRoute =<< getUnderlying)
      mbib <- fmap itemBody <$> optional (load $ fromFilePath $ replaceExtension fp "bib")
      style <- unsafeCompiler . readCSLFile . Hakyll.toFilePath . itemIdentifier
                  =<< load (fromFilePath $ replaceExtension fp "csl")
                  <|> (load "default.csl" :: Compiler (Item CSL))
      ipandoc <- mapM (unsafeCompiler . texToMarkdown) =<< getResourceBody
      let ip' = case mbib of
                  Just (BibTeX bib) | not (null bib) ->
                      fmap (processCites style bib . appendBiblioSection) ipandoc
                  _ -> ipandoc
          item = writePandocWith
                     def{ writerHTMLMathMethod = MathJax "http://konn-san.com/math/mathjax/MathJax.js?config=xypic"}
                     $ fmap (addPDFLink ("/" </> replaceExtension fp "pdf") . addAmazonAssociateLink "konn06-22") ip'
      saveSnapshot "content" =<< relativizeUrls =<< applyDefaultTemplate item

  match ("math/**.tex") $ version "pdf" $ do
    route $ setExtension "pdf"
    compile $ getResourceBody >>= compileToPDF

  match ("math/**.png") $
    route idRoute >> compile copyFileCompiler

  match ("math/**.md" .||. "prog/**.md" .||. "writing/**.md") $ do
    route $ setExtension "html"
    compile $
       myPandocCompiler >>= saveSnapshot "content" >>= applyDefaultTemplate >>= relativizeUrls

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots subContentsWithoutIndex "content"
        >>= myRecentFirst
        >>= return . take 10 . filter (matches ("index.md" .||. complement "**/index.md") . itemIdentifier)
        >>= renderAtom feedConf feedCxt

addPDFLink :: FilePath -> Pandoc -> Pandoc
addPDFLink plink (Pandoc meta body) = Pandoc meta body'
  where
    Pandoc _ body' = doc $ mconcat [ para $ "[" <> link (encodeString plink) "PDF版" "PDF版" <> "]"
                                   , Pan.fromList body
                                   ]

appendBiblioSection :: Pandoc -> Pandoc
appendBiblioSection (Pandoc meta bs) =
    Pandoc meta $ bs ++ [Header 1 ("biblio", [], []) [Str "参考文献"]]

listChildren :: Bool -> Compiler [Item String]
listChildren recursive = do
  ident <- getUnderlying
  let Just dir = stripPrefix "." $ directory $ toFilePath ident
      exts = ["md", "tex"]
      wild = if recursive then "**" else "*"
      pat =  foldr1 (.||.) ([fromGlob $ encodeString $ dir </> wild <.> e | e <- exts])
               .&&. complement (fromList [ident] .||. hasVersion "pdf")
  loadAll pat >>= myRecentFirst

compileToPDF :: Item String -> Compiler (Item TmpFile)
compileToPDF item = do
  TmpFile (decodeString -> texPath) <- newTmpFile "pdflatex.tex"
  let tmpDir  = directory texPath
      dviPath = filename $ replaceExtension texPath "dvi"
      pdfPath = filename $ replaceExtension texPath "pdf"
      bibOrig = replaceExtension (toFilePath (itemIdentifier item)) "bib"

  unsafeCompiler $ shelly $ silently $ do
    writefile texPath $ T.pack $ itemBody item
    exts <- test_e =<< absPath bibOrig
    when exts $ do
      cp  bibOrig $ tmpDir </> filename bibOrig
    cd tmpDir
    cmd "latexmk" $ filename texPath
    cmd "dvipdfmx" "-o" pdfPath dviPath
    return ()
  makeItem $ TmpFile $ encodeString (tmpDir </> pdfPath)

renderDocIndex :: [Item String] -> Compiler String
renderDocIndex is = do
  is' <- forM is $ \i -> ( , itemBody i) . fromJust <$> getRoute (itemIdentifier i)
  return $ renderHtml $ forM_ is' $ \(path, src) ->
      let tags = parseTags src
          content = innerText $ getTagContent "div" (anyAttrLit ("class", "doc")) tags
          name = takeWhile (/= ':') $ innerText $
            getTagContent "div" (anyAttrLit ("class", "description")) tags
      in [shamlet| <dt>
                     <a href="#{path}">#{name}
                   <dd>#{content}
                 |]

renderMeta :: [Inline] -> String
renderMeta ils = writeHtmlString def $ Pandoc nullMeta [Plain ils]

subContentsWithoutIndex :: Pattern
subContentsWithoutIndex = ("**.md" .||. ("math/**.tex" .&&. hasVersion "html"))
                     .&&. complement ("index.md" .||. "**/index.md" .||. "archive.md")

feedCxt :: Context String
feedCxt =  mconcat [ field "published" itemDateStr
                   , field "updated" itemDateStr
                   , bodyField "description"
                   , defaultContext
                   ]

itemDateStr :: Item a -> Compiler String
itemDateStr = fmap (formatTime defaultTimeLocale "%Y/%m/%d %X %Z") . itemDate

feedConf :: FeedConfiguration
feedConf = FeedConfiguration { feedTitle = "konn-san.com 建設予定地"
                             , feedDescription = "数理論理学を中心に数学、Haskell、推理小説、評論など。"
                             , feedAuthorName = "Hiromi ISHII"
                             , feedAuthorEmail = ""
                             , feedRoot = "http://konn-san.com"
                             }

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransform def def{ writerHTMLMathMethod = MathJax "http://konn-san.com/math/mathjax/MathJax.js?config=xypic"} (addAmazonAssociateLink "konn06-22")

applyDefaultTemplate :: Item String -> Compiler (Item String)
applyDefaultTemplate = applyDefaultTemplateWith

applyDefaultTemplateWith :: Item String -> Compiler (Item String)
applyDefaultTemplateWith =
  let navbar = field "navbar" $ return . makeNavBar . itemIdentifier
      bcrumb = field "breadcrumb" $ makeBreadcrumb
      date = field "date" itemDateStr
      children = field "children" $ const $ do
        chs <- listChildren False
        navs <- liftM catMaybes . forM chs $ \c -> do
          link  <- getRoute (itemIdentifier c)
          title <- getMetadataField (itemIdentifier c) "title"
          return $ (,) <$> link <*> title
        return $ renderHtml [shamlet| <ul>
                                        $forall (pth, title) <- navs
                                          <li>
                                            <a href="#{pth}">
                                               #{title}
                                    |]
      header = field "head" $ \i -> return $
        if "math" `isPrefixOf` Hakyll.toFilePath (itemIdentifier i) && "math/index.md" /= toFilePath (itemIdentifier i)
        then renderHtml [shamlet|<link rel="stylesheet" href="/css/math.css">|]
        else ""
      meta = field "meta" $ liftM mconcat . forM [("description", "description"), ("tag", "Keywords")] . extractMeta
      cxt  = (defaultContext <> navbar <> bcrumb <> header <> meta <> children <> date)
  in return . fmap (demoteHeaders . withTags addTableClass)
         >=> applyAsTemplate cxt
         >=> loadAndApplyTemplate "templates/default.html" cxt

extractMeta :: Item String -> (String, String) -> Compiler String
extractMeta i (from, to) = do
  mt <- getMetadataField (itemIdentifier i) from
  case mt of
    Nothing -> return ""
    Just st -> return $ renderHtml $ [shamlet|<meta name=#{to} content=#{st} />|]

addTableClass :: Tag String -> Tag String
addTableClass (TagOpen "table" attr) = TagOpen "table" (("class", "table"):attr)
addTableClass t = t

config :: Configuration
config = defaultConfiguration { deployCommand = "rsync --checksum -av _site/* sakura-vps:~/mighttpd/public_html/ && git add img math writing prog && git commit -am\"deployed\" && git push origin master"}

addAmazonAssociateLink :: String -> Pandoc -> Pandoc
addAmazonAssociateLink = bottomUp . procAmazon

procAmazon :: String -> Inline -> Inline
procAmazon tag (Link is (url, title))  = Link is (attachTo tag url, title)
procAmazon tag (Image is (url, title)) = Image is (attachTo tag url, title)
procAmazon _   il                      = il

attachTo :: String -> String -> String
attachTo key url
    | (p@("http:":"":amazon:paths), qs) <- decodePath (BS.pack url)
    , amazon `elem` amazons
    , let cipath = map CI.mk paths
    , ["o", "asin"] `isPrefixOf` cipath || "dp" `elem` cipath
                        || ["gp", "product"] `isPrefixOf` cipath
    , isNothing (lookup "tag" qs)
         = tail $ BS.unpack $ toByteString $ encodePath p (("tag", Just $ BS.pack key):qs)
    | Just as <- T.stripPrefix "asin:" $ T.pack url
    = "http://www.amazon.co.jp/dp/" ++ T.unpack as ++ "/?tag=" ++ key
attachTo _   url = url

amazons :: [T.Text]
amazons = "www.amazon.com":"amazon.com":concatMap (\cc -> [T.concat [www,"amazon.",co,cc] | www <- ["","www."], co <- ["co.", ""]]) ccTLDs

ccTLDs :: [T.Text]
ccTLDs = ["jp"]

catDic :: [(Html, String)]
catDic = [("Home", "/")
         ,("Math", "/math")
         ,("Programming", "/prog")
         ,("Writings", "/writing")
         ,("Archive", "/archive.html")
         ,("Blog", "http://blog.konn-san.com/")
         ,("Tumblr", "http://tumblr.konn-san.com/")
         ]

getActive :: Identifier -> String
getActive "archive.md" = "/archive.html"
getActive ident = fromMaybe "/" $ listToMaybe $ filter p $ map snd catDic
  where
    p "/" = False
    p ('/':inp) = fromGlob (inp++"/**") `matches` ident
    p _ = False

makeBreadcrumb :: Item String -> Compiler String
makeBreadcrumb item = do
  let ident = itemIdentifier item
  mytitle <- getMetadataField' ident "title"
  let parents = filter (/= toFilePath ident) $ map ((</> "index.md"). Path.concat) $ init $ inits $ splitDirectories $ toFilePath ident
  bc <- liftM catMaybes . forM parents $ \fp -> do
    mpath <- liftM toUrl <$> getRoute (fromFilePath fp)
    mtitle <-  getMetadataField (fromFilePath fp) "title"
    return $ (,) <$> mpath <*> mtitle
  return $ renderHtml [shamlet|
      <ul .breadcrumb>
        $forall (path, title) <- bc
          <li>
            <a href=#{path}>#{title}
            <span .divider>/
        <li .active>
          #{mytitle}
    |]

makeNavBar :: Identifier -> String
makeNavBar ident = renderHtml $ do
  let cats = [(path, cat, getActive ident == path) | (cat, path) <- catDic ]
  [shamlet|
  <div .navbar .navbar-inverse .navbar-fixed-top>
    <div .navbar-inner>
      <div .container>
        <button .btn .btn-navbar data-toggle="collapse" data-target=".nav-collapse">
          $forall _ <- catDic
            <span .icon-bar>
        <a .brand href="/">konn-san.com
        <div .nav-collapse .collapse>
          <ul .nav>
            $forall (path, cat, isActive) <- cats
              $if isActive
                 <li .active>
                   <a href="#{path}">#{cat}
              $else
                 <li>
                   <a href="#{path}">#{cat}
  |]

readHierarchy :: String -> [(String, String)]
readHierarchy = mapMaybe (toTup . words) . lines
  where
    toTup (x:y:ys) = Just (y ++ unwords ys, x)
    toTup _        = Nothing

postList :: Maybe Int -> Pattern -> Compiler String
postList mcount pat = do
  postItemTpl <- loadBody "templates/update.html"
  posts <- fmap (maybe id take mcount) . myRecentFirst =<< loadAll pat
  let myDateField = field "date" itemDateStr
      pdfField = field "pdf" itemPDFLink
  applyTemplateList postItemTpl (pdfField <> myDateField <> defaultContext) posts

itemPDFLink :: Item a -> Compiler String
itemPDFLink item
    | "**.tex" `matches` itemIdentifier item = do
        Just route <- getRoute $ itemIdentifier item
        return $ concat $ [" [", "<a href=\""
                          , encodeString $ replaceExtension (decodeString route) "pdf"
                          , "\">"
                          , "PDF版"
                          , "</a>"
                          , "]"]
    | otherwise                                           = return ""

myRecentFirst :: [Item a] -> Compiler [Item a]
myRecentFirst is = do
  ds <- mapM itemDate is
  return $ map snd $ sortBy (flip $ comparing (zonedTimeToLocalTime . fst)) $ zip ds is

itemDate :: Item a -> Compiler ZonedTime
itemDate item = do
  let ident = itemIdentifier item
  dateStr <- getMetadataField ident "date"
  let mdate = dateStr >>= parseTime defaultTimeLocale "%Y/%m/%d %X %Z"
  case mdate of
    Just date -> return date
    Nothing -> do
      unsafeCompiler $ utcToLocalZonedTime =<< getModified (toFilePath ident)
