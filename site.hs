{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, TupleSections #-}
module Main where
import           Blaze.ByteString.Builder        (toByteString)
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8           as BS
import qualified Data.CaseInsensitive            as CI
import           Data.List                       hiding (span)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import qualified Data.Text                       as T
import           Data.Time
import           Filesystem
import qualified Filesystem.Path.CurrentOS       as Path
import           Hakyll
import           MathConv
import           Network.HTTP.Types
import           Prelude                         hiding (div, span)
import           System.Cmd
import           System.FilePath
import           System.Locale
import           Text.Blaze.Html.Renderer.String
import           Text.Hamlet
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Pandoc
import           Text.Pandoc.Builder             hiding (fromList)
import qualified Text.Pandoc.Builder             as Pan

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
  match ("math/**.tex") $ version "html" $ do
    route $ setExtension "html"
    compile $ do
      fp <- fromJust <$> (getRoute =<< getUnderlying)
      ipandoc <- fmap (addPDFLink ("/" </> replaceExtension fp "pdf") . addAmazonAssociateLink "konn06-22" . texToMarkdown)
                   <$> getResourceBody
      let item = writeHtmlString def{ writerHTMLMathMethod = MathJax "http://konn-san.com/math/mathjax/MathJax.js?config=xypic"}
                   <$> ipandoc
      saveSnapshot "content" item
      applyDefaultTemplate item >>= relativizeUrls

  match ("math/**.tex") $ version "pdf" $ do
    route $ setExtension "pdf"
    compile $ do
      getResourceBody >>= compileToPDF

  match ("math/mathjax/**") $
     route idRoute >> compile copyFileCompiler

  match ("math/**.png" .&&. complement "math/mathjax/**") $
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

addPDFLink :: String -> Pandoc -> Pandoc
addPDFLink plink (Pandoc meta body) = Pandoc meta body'
  where
    Pandoc _ body' = doc $ mconcat [ para $ "[" <> link plink "PDF版" "PDF版" <> "]"
                                   , Pan.fromList body
                                   ]

listChildren :: Bool -> Compiler [Item String]
listChildren recursive = do
  ident <- getUnderlying
  let dir  = takeDirectory $ toFilePath ident
      exts = ["md", "tex"]
      wild = if recursive then "**" else "*"
      pat =  foldr1 (.||.) ([fromGlob $ dir </> wild <.> e | e <- exts])
               .&&. complement (fromList [ident])
  loadAll pat >>= myRecentFirst

compileToPDF :: Item String -> Compiler (Item TmpFile)
compileToPDF item = do
  TmpFile texPath <- newTmpFile "pdflatex.tex"
  let tmpDir  = takeDirectory texPath
      dviPath = replaceExtension texPath "dvi"
      pdfPath = replaceExtension texPath "pdf"

  unsafeCompiler $ do
    Prelude.writeFile texPath $ itemBody item
    _ <- system $ unwords ["platex"
                          , "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
    _ <- system $ unwords ["dvipdfmx", "-o", pdfPath, dviPath, ">/dev/null", "2>&1"]
    return ()
  makeItem $ TmpFile pdfPath

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
renderMeta ils = writeHtmlString def $ Pandoc (Meta [] [] []) [Plain ils]

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
        if "math" `isPrefixOf` toFilePath (itemIdentifier i) && "math/index.md" /= toFilePath (itemIdentifier i)
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
  Just mytitle <- getMetadataField ident "title"
  let parents = filter (/= toFilePath ident) $ map ((</> "index.md").joinPath) $ init $ inits $ splitPath $ toFilePath ident
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
  applyTemplateList postItemTpl (myDateField <> defaultContext) posts

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
      unsafeCompiler $ utcToLocalZonedTime =<< getModified (Path.decodeString $ toFilePath ident)
