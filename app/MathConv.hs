{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module MathConv
  ( preprocessLaTeX,
    texToMarkdown,
    PreprocessedLaTeX (..),
  )
where

import Control.Arrow (left)
import Control.Exception (SomeException)
import Control.Lens hiding (rewrite)
import Control.Lens.Extras (is)
import qualified Control.Monad.Catch as E
import Control.Monad.Identity
import Control.Monad.State.Strict
  ( MonadState,
    StateT,
    evalStateT,
    gets,
    modify,
    runState,
    runStateT,
  )
import Control.Monad.Writer.Strict (runWriter, tell)
import Data.Char
  ( isAlpha,
    isAlphaNum,
    isAscii,
    isLatin1,
    isLetter,
    isSpace,
  )
import Data.Default
import Data.Either (fromRight)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Store (Store)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lens (unpacked)
import Data.Typeable
import Development.Shake
import GHC.Generics (Generic)
import Instances ()
import Language.Haskell.TH (litE)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Lib (stringL)
import Lenses
import Macro
import qualified MyTeXMathConv as MyT
import System.Directory (getHomeDirectory, makeAbsolute)
import System.FilePath
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (img, object, toValue, (!))
import Text.Blaze.Html5.Attributes
  ( alt,
    class_,
    data_,
    src,
    type_,
  )
import Text.LaTeX.Base hiding ((&))
import Text.LaTeX.Base.Class (comm1, fromLaTeX)
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.CrossRef
  ( LabelFormat (ThisCounter),
    Numeral (Arabic),
    RefItem (Item),
    RefOptions (..),
    procCrossRef,
  )
import qualified Text.LaTeX.CrossRef as R
import Text.Pandoc hiding (MathType, Writer)
import Text.Pandoc.Shared
import qualified Text.Parsec as P
import Text.Regex.Applicative (RE, psym, replace, (<|>))
import Text.TeXMath.Readers.TeX.Macros
import Utils
import Web.Sake (dropDirectory1)
import Web.Sake.Class

default (String, T.Text, Integer)

data MachineState = MachineState
  { _macroDefs :: [Macro]
  , _imgPath :: FilePath
  }
  deriving (Show)

makeLenses ''MachineState

data PreprocessedLaTeX = PreprocessedLaTeX
  { latexSource :: Text
  , images :: Maybe (Int, Text)
  }
  deriving (Read, Show, Eq, Ord, Generic, Store, Typeable)

instance Writable PreprocessedLaTeX where
  writeTo_ fp = writeTo_ fp . Binary

preprocessLaTeX :: TeXMacros -> Text -> PreprocessedLaTeX
preprocessLaTeX macs tsrc =
  case parseLaTeX tsrc of
    Right lat0 ->
      let ltree = procCrossRef myCrossRefConf $ rewriteEnvAndBrakets lat0
          tlibs = extractTikZLib ltree
          (lat, localMacros) = extractMacros ltree
          preamble = tlibs ++ localMacros
          macros = macs <> parseTeXMacros localMacros
          (l, st) = runState (extractTikZ $ applyTeXMacro macros lat) mempty
          proc'd = render l
          diags = render $ buildTikzer preamble $ toList st
          imgs
            | Seq.null st = Nothing
            | otherwise = Just (Seq.length st, diags)
       in PreprocessedLaTeX proc'd imgs
    Left _ -> PreprocessedLaTeX tsrc Nothing

extractTikZLib :: LaTeX -> [LaTeX]
extractTikZLib = queryWith go
  where
    go c@(TeXComm "usetikzlibrary" _) = [c]
    go c@(TeXComm "tikzset" _) = [c]
    go c@(TeXComm "pgfplotsset" _) = [c]
    go _ = []

extractMacros :: LaTeX -> (LaTeX, [LaTeX])
extractMacros = runWriter . transformM go
  where
    go c@(TeXComm "newcommand" _) = TeXEmpty <$ tell [c]
    go c@(TeXComm "renewcommand" _) = TeXEmpty <$ tell [c]
    go c@(TeXComm "newcommand*" _) = TeXEmpty <$ tell [c]
    go c@(TeXComm "renewcommand*" _) = TeXEmpty <$ tell [c]
    go c = return c

type Machine = StateT MachineState PandocIO

myReaderOpts :: ReaderOptions
myReaderOpts =
  def
    { readerExtensions =
        extensionsFromList exts
          <> pandocExtensions
    }
  where
    exts =
      [ Ext_raw_html
      , Ext_latex_macros
      , Ext_raw_attribute
      , Ext_raw_tex
      , Ext_tex_math_dollars
      ]

parseTeX :: Text -> Either String LaTeX
parseTeX = left show . P.runParser latexParser defaultParserConf ""

home :: FilePath
home = $(litE . stringL =<< TH.runIO getHomeDirectory)

absBib :: FilePath
absBib = home </> "Library/texmf/bibtex/bib/myreference.bib"

rewriteBib :: FilePath -> LaTeX -> LaTeX
rewriteBib dir = bottomUp loop
  where
    loop (TeXComm "addbibresource" [FixArg "myreference.bib"]) =
      TeXComm "addbibresource" [FixArg $ TeXRaw $ T.pack absBib]
    loop (TeXComm "addbibresource" [FixArg fp])
      | isRelative (T.unpack $ render fp) =
        TeXComm
          "addbibresource"
          [ FixArg $
              TeXRaw $
                T.pack $ dir </> T.unpack (render fp)
          ]
    loop t = t

texToMarkdown :: TeXMacros -> FilePath -> Text -> IO Pandoc
texToMarkdown defMacros fp src_ = do
  absFP <- makeAbsolute $
    case L.stripPrefix "_site/" fp of
      Nothing -> fp
      Just rest -> "site-src" </> rest
  let setupResource = do
        srcs <- getResourcePath
        setResourcePath $
          [ home </> "Library/texmf/tex/platex"
          , home </> "Library/texmf/bibtex/bib"
          , takeDirectory absFP
          ]
            ++ srcs
  macros <-
    liftIO $
      fst . parseMacroDefinitions
        <$> T.readFile "/Users/hiromi/Library/texmf/tex/platex/mystyle.sty"
  let ltree0 =
        applyTeXMacro defMacros $
          rewriteBib (takeDirectory absFP) $
            view _Right $
              parseTeX $ applyMacros macros src_
      initial =
        applyMacros macros $
          render $
            rewriteEnvAndBrakets ltree0
      st0 =
        MachineState
          { _macroDefs = macros -- ++ lms
          , _imgPath = dropDirectory1 $ dropExtension fp
          }
  mabs <-
    either (const Nothing) ((^? _MetaBlocks) <=< M.lookup "abstract" . unMeta . getMeta)
      <$> runIO (setupResource >> readLaTeX myReaderOpts initial)
  pan <- runIOorExplode $ do
    setupResource
    (p0@(Pandoc meta0 bdy), s0) <- runStateT (texToMarkdownM initial) st0
    case mabs of
      Nothing -> return p0
      Just bs -> do
        let ps0 = Pandoc meta0 bs
        asrc <- fromRight "" <$> E.try @_ @SomeException (writeLaTeX def ps0)
        Pandoc _ abbs <- evalStateT (texToMarkdownM asrc) s0
        return $ Pandoc (Meta $ M.insert "abstract" (MetaBlocks abbs) $ unMeta meta0) bdy

  return $ adjustJapaneseSpacing pan

getMeta :: Pandoc -> Meta
getMeta (Pandoc m _) = m

texToMarkdownM :: Text -> Machine Pandoc
texToMarkdownM s = do
  mcs <- use macroDefs
  lat <-
    readLaTeX myReaderOpts $
      applyMacros mcs s
  procTikz =<< rewriteEnv lat

adjustJapaneseSpacing :: Pandoc -> Pandoc
adjustJapaneseSpacing = bottomUp procMathBoundary . bottomUp procStr
  where
    procMathBoundary =
      replace (insertBoundary (Str " ") (is _Math) (isStrStarting japaneseLetter))
        . replace (insertBoundary (Str " ") (isStrEnding japaneseLetter) (is _Math))
    procStr =
      _Str . unpacked
        %~ replace (insertBoundary ' ' isAsciiAlphaNum japaneseLetter)
          . replace (insertBoundary ' ' japaneseLetter isAsciiAlphaNum)

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = (isAscii c && isAlphaNum c) || isLatin1 c || isAlpha c

japaneseLetter :: Char -> Bool
japaneseLetter c = not (isLatin1 c || isAscii c || isAlpha c) && isLetter c

isStrStarting :: (Char -> Bool) -> Inline -> Bool
isStrStarting p = maybe False p . preview (_Str . _head)

isStrEnding :: (Char -> Bool) -> Inline -> Bool
isStrEnding p = maybe False p . preview (_Str . _last)

insertBoundary :: t -> (t -> Bool) -> (t -> Bool) -> RE t [t]
insertBoundary c p q = boundary p q <&> \(l, r) -> [l, c, r]

boundary :: (a -> Bool) -> (a -> Bool) -> RE a (a, a)
boundary p q = (,) <$> psym p <*> psym q

rewriteEnvAndBrakets :: LaTeX -> LaTeX
rewriteEnvAndBrakets = bottomUp rewrite . bottomUp alterEnv
  where
    expands = ["Set", "Braket"]
    alterEnv (TeXEnv env args body)
      | Just env' <- lookup env envAliases = TeXEnv env' args body
    alterEnv e = e
    rewrite (TeXComm comm [FixArg src_]) | comm `elem` expands =
      case breakTeXOn "|" src_ of
        Just (lhs, rhs) -> TeXComm comm [FixArg lhs, FixArg rhs]
        Nothing -> TeXComm (T.unpack $ T.toLower $ T.pack comm) [FixArg src_]
    rewrite t = t

splitTeXOn :: Text -> LaTeX -> [LaTeX]
splitTeXOn delim t =
  case breakTeXOn delim t of
    Nothing -> [tex]
    Just (a, b) -> a : splitTeXOn delim b

breakTeXOn :: T.Text -> LaTeX -> Maybe (LaTeX, LaTeX)
breakTeXOn _ TeXEmpty = Nothing
breakTeXOn s (TeXRaw t) =
  case T.breakOn s t of
    (_, "") -> Nothing
    answer ->
      answer & _2 %~ T.drop 1
        & both %~ TeXRaw
        & Just
breakTeXOn s (TeXSeq l r) =
  do
    (l0, l1) <- breakTeXOn s l
    return (l0, TeXSeq l1 r)
    <|> do
      (r0, r1) <- breakTeXOn s r
      return (TeXSeq l r0, r1)
breakTeXOn _ _ = Nothing

myCrossRefConf :: RefOptions
myCrossRefConf =
  RefOptions
    { subsumes = HM.empty
    , formats =
        HM.fromList
          [(Item 1, [R.Str "(", ThisCounter Arabic, R.Str ")"])]
    , numberedEnvs = HS.fromList $ map T.pack $ L.delete "proof" envs
    , remainLabel = True
    , useHyperlink = True
    }

envs :: [String]
envs =
  [ "prop"
  , "proof"
  , "theorem"
  , "lemma"
  , "axiom"
  , "remark"
  , "exercise"
  , "definition"
  , "question"
  , "answer"
  , "problem"
  , "corollary"
  , "fact"
  , "conjecture"
  , "claim"
  , "subproof"
  , "notation"
  ]

envAliases :: [(String, String)]
envAliases =
  [ ("enumerate*", "enumerate!")
  , ("itemize*", "itemize")
  ]

commandDic :: [(String, Either ([Inline] -> Inline) Text)]
commandDic =
  [ ("underline", Right "u")
  , ("bf", Left Strong)
  , ("emph", Left Strong)
  , ("textgt", Left Strong)
  , ("textsf", Left Strong)
  ]

rewriteInlineCmd :: [Inline] -> Machine [Inline]
rewriteInlineCmd = fmap concat . mapM step
  where
    step (RawInline "latex" src_)
      | Right t <- parseTeX src_ = rewrite t
    step i = return [i]
    rewrite (TeXSeq l r) = (++) <$> rewrite l <*> rewrite r
    rewrite (TeXComm "parpic" args) = procParpic args
    rewrite (TeXComm "label" [FixArg lab]) =
      return [Link (render lab, [], []) [] ("", "")]
    rewrite (TeXComm "ruby" [FixArg rb, FixArg rt]) = do
      rubyBody <- concat <$> mapM step (inlineLaTeX (render rb))
      rubyText <- concat <$> mapM step (inlineLaTeX (render rt))
      return $
        [RawInline "html" "<ruby><rb>"]
          ++ rubyBody
          ++ [RawInline "html" "</rb><rp>（</rp><rt>"]
          ++ rubyText
          ++ [RawInline "html" "</rt><rp>）</rp><ruby>"]
    rewrite c@(TeXComm cm [FixArg arg0]) = do
      arg <- rewrite arg0
      case lookup cm commandDic of
        Just (Right t) ->
          return $
            [RawInline "html" $ "<" <> t <> ">"]
              ++ arg
              ++ [RawInline "html" $ "</" <> t <> ">"]
        Just (Left inl) -> return [inl arg]
        _ -> return $ inlineLaTeX $ render c
    rewrite c = return $ inlineLaTeX $ render c

data Align = AlignL | AlignR
  deriving (Read, Show, Eq, Ord)

procParpic :: [TeXArg] -> Machine [Inline]
procParpic [OptArg "r", FixArg lat] = procParpic' AlignR lat
procParpic [OptArg "l", FixArg lat] = procParpic' AlignL lat
procParpic (fixArgs -> [lat]) = procParpic' AlignR lat
procParpic _ = return []

procParpic' :: Align -> LaTeX -> Machine [Inline]
procParpic' al lat = do
  let pull = case al of
        AlignL -> "pull-left"
        AlignR -> "pull-right"
  pure . Span ("", ["media", pull], []) . concatMap getInlines . pandocBody
    <$> texToMarkdownM (render lat)

getInlines :: Block -> [Inline]
getInlines (Plain b) = b
getInlines (LineBlock b) = concat b
getInlines (Para b) = b
getInlines (CodeBlock lang b2) = [Code lang b2]
getInlines (RawBlock lang b) = [RawInline lang b]
getInlines (BlockQuote b) = concatMap getInlines b
getInlines (OrderedList _ b2) = concatMap (concatMap getInlines) b2
getInlines (BulletList b) = concatMap (concatMap getInlines) b
getInlines (DefinitionList b) = concat [lls ++ concatMap (concatMap getInlines) bs | (lls, bs) <- b]
getInlines (Header _ attr b3) = [Span attr b3]
getInlines HorizontalRule = []
getInlines Table {} = []
getInlines (Div b1 b2) = [Span b1 $ concatMap getInlines b2]
getInlines Null = []

-- traced :: Show a => String -> a -> a
-- traced lab a = DT.trace (lab <> ": " <> show a) a

pandocBody :: Pandoc -> [Block]
pandocBody (Pandoc _ body) = body

fixArgs :: Foldable f => f TeXArg -> [LaTeX]
fixArgs = toListOf (folded . _FixArg)

inlineLaTeX :: Text -> [Inline]
inlineLaTeX src_ =
  let Pandoc _ body =
        either (const (Pandoc nullMeta [])) id $
          runPure $
            readLaTeX myReaderOpts src_
   in concatMap getInlines body

rewriteEnv :: Pandoc -> Machine Pandoc
rewriteEnv (Pandoc meta bs) =
  Pandoc meta <$> (bottomUpM rewriteInlineCmd =<< rewriteBeginEnv (bottomUp amendAlignat bs))

rewriteBeginEnv :: [Block] -> Machine [Block]
rewriteBeginEnv = concatMapM step
  where
    step :: Block -> Machine [Block]
    step (RawBlock "latex" src_)
      | Right (TeXEnv "enumerate!" args body) <- parseTeX src_ =
        pure <$> procEnumerate args body
      | Right (TeXEnv env0 args body) <- parseTeX src_
        , Just env <- lookupCustomEnv (T.pack env0) (map T.pack envs) = do
        let divStart
              | null args = mconcat ["<div class=\"", env, "\">"]
              | otherwise =
                mconcat
                  [ "<div class=\""
                  , env
                  , "\" data-theorem-name=\""
                  , T.unwords $ map texToEnvNamePlainString args
                  , "\">"
                  ]
        Pandoc _ myBody <- texToMarkdownM $ render body
        return $ RawBlock "html" divStart : myBody ++ [RawBlock "html" "</div>"]
    step b = return [b]

lookupCustomEnv :: Text -> [Text] -> Maybe Text
lookupCustomEnv e es =
  e <$ guard (e `elem` es)
    <|> e <> "-plain" <$ guard (e <> "*" `elem` es)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f a = concat <$> mapM f a

procEnumerate :: [TeXArg] -> LaTeX -> Machine Block
procEnumerate args body = do
  ~(Pandoc _ [OrderedList _ blcs]) <-
    rewriteEnv
      =<< lift
        ( readLaTeX myReaderOpts $
            render $ TeXEnv "enumerate" [] body
        )
  return $ OrderedList (parseEnumOpts args) blcs

-- tr :: Show a => String -> a -> a
-- tr lab s = DT.trace (lab <> ": " <> show s) s

splitLeftMostBrace :: LaTeX -> Maybe (LaTeX, LaTeX)
splitLeftMostBrace = loop Nothing
  where
    isEmpty TeXEmpty = True
    isEmpty (TeXRaw t) = T.all isSpace t
    isEmpty TeXComment {} = True
    isEmpty _ = False
    loop mrest (TeXBraces t) = Just (t, fromMaybe TeXEmpty mrest)
    loop mrest (TeXSeq l r)
      | isEmpty l = loop mrest r
      | isEmpty r = loop mrest l
      | otherwise = loop (Just $ maybe r (TeXSeq r) mrest) l
    loop _ _ = Nothing

-- Fixes long-standing bad behaviour of parsing alignat(*) in Pandoc parser.
amendAlignat :: Inline -> Inline
amendAlignat (Math DisplayMath tsrc)
  | Right (TeXEnv (T.pack -> env) [] body) <- parseLaTeX tsrc
    , env `elem` ["aligned", "aligned*"]
    , Just (TeXRaw nums, body') <- splitLeftMostBrace body
    , [(i :: Int, "")] <- reads $ T.unpack (T.strip nums) =
    let envedName = T.replace "ed" "edat" env
        args = [FixArg $ TeXRaw $ T.pack $ show i]
     in Math DisplayMath $ render $ TeXEnv (T.unpack envedName) args body'
amendAlignat i = i

parseEnumOpts :: [TeXArg] -> ListAttributes
parseEnumOpts args =
  let opts =
        [ (render key, render val)
        | OptArg lat <- args
        , opt <- splitTeXOn "," lat
        , Just (key, val) <- [breakTeXOn "=" opt]
        ]
      styleDic =
        [ ("arabic", Decimal)
        , ("Alph", UpperAlpha)
        , ("alph", LowerAlpha)
        , ("Roman", UpperRoman)
        , ("roman", LowerRoman)
        ]
      labF = fromMaybe "" $ lookup "label" opts
      start = maybe 1 (read . T.unpack) $ lookup "start" opts
      style =
        fromMaybe Decimal $
          listToMaybe
            [sty | (com, sty) <- styleDic, ("\\" <> com) `T.isInfixOf` labF]
      oparens = T.count "(" labF
      cparens = T.count ")" labF
      delim
        | max oparens cparens >= 2 = TwoParens
        | max oparens cparens == 1 = OneParen
        | "." `T.isInfixOf` labF = Period
        | otherwise = DefaultDelim
   in (start, style, delim)

buildTikzer :: [LaTeX] -> [LaTeX] -> LaTeX
buildTikzer tikzLibs tkzs = snd $
  runIdentity $
    runLaTeXT $ do
      fromLaTeX $ TeXComm "RequirePackage" [FixArg "luatex85"]
      documentclass ["tikz", "preview"] "standalone"
      usepackage ["hiragino-pron"] "luatexja-preset"
      usepackage [] "amsmath"
      usepackage [] "amssymb"
      usepackage [] "pgfplots"
      usepackage [] "mymacros"
      comm1 "usetikzlibrary" "matrix,arrows,backgrounds,calc,shapes"
      mapM_ fromLaTeX tikzLibs
      document $ mapM_ textell tkzs

extractTikZ :: MonadState (Seq LaTeX) m => LaTeX -> m LaTeX
extractTikZ = bottomUpM step
  where
    step t@(TeXEnv "tikzpicture" _ _) = save t
    step (TeXComm "tikz" args) =
      case splitAt (length args - 1) args of
        (opts, [FixArg l]) -> save $ TeXEnv "tikzpicture" opts l
    step t = return t

    save t = do
      n <- gets length
      modify (|> t)
      return $ TeXComm generatedGraphicPlaceholder [FixArg $ TeXRaw $ T.pack $ show n]

generatedGraphicPlaceholder :: String
generatedGraphicPlaceholder = "generatedgraphic"

procTikz :: Pandoc -> Machine Pandoc
procTikz = bottomUpM step
  where
    step (RawBlock "latex" src_)
      | Right ts <- parseTeX src_ =
        fmap Plain $
          forM
            [ nth
            | (TeXComm c [FixArg nth]) <- universe ts
            , c == generatedGraphicPlaceholder
            ]
            $ \nth -> do
              liftIO $ putStrLn $ "generated image: " ++ T.unpack (render nth)
              let n = fromMaybe 0 $ readMaybe $ T.unpack $ render nth
              fp <- use imgPath
              let dest = toValue $ ("/" :: String) </> fp </> ("image-" ++ show n ++ ".svg")
                  alts = toValue $ ("/" :: String) </> fp </> ("image-" ++ show n ++ ".png")
              return $
                Span
                  ("", ["img-fluid"], [])
                  [ RawInline "html" $
                      LT.toStrict $
                        renderHtml $
                          object ! class_ "img-thumbnail media-object"
                            ! type_ "image/svg+xml"
                            ! data_ dest
                            $ img ! src alts ! alt "Diagram"
                  ]
    step a = return a

texToEnvNamePlainString :: TeXArg -> Text
texToEnvNamePlainString str =
  let cated =
        case str of
          FixArg l -> render l
          OptArg l -> render l
          SymArg l -> render l
          ParArg l -> render l
          MOptArg lats -> T.intercalate "," $ map render lats
          MSymArg lats -> T.intercalate "," $ map render lats
          MParArg lats -> T.intercalate "," $ map render lats
   in either (const $ render str) (stringify . bottomUp go) $
        runPure $ readLaTeX myReaderOpts cated
  where
    go :: Inline -> Inline
    go = bottomUp $ \case
      Math _ eqn -> Str $ stringify $ MyT.readTeXMath eqn
      t -> t
