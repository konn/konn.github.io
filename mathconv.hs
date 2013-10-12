{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, StandaloneDeriving        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MathConv where
import           Control.Applicative
import           Control.Lens           hiding (op, rewrite)
import qualified Data.Attoparsec.Text   as Atto
import           Data.Data
import           Data.Default
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Text.LaTeX.Base        hiding ((&))
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           Text.Pandoc            hiding (MathType)
import           Text.TeXMath.Macros
import           Text.TeXMath.ToUnicode
import           Text.TeXMath.Types

deriving instance Typeable Measure
deriving instance Data Measure
deriving instance Typeable TeXArg
deriving instance Data TeXArg
deriving instance Typeable MathType
deriving instance Data MathType
deriving instance Typeable LaTeX
deriving instance Data LaTeX


myReaderOpts :: ReaderOptions
myReaderOpts = def { readerExtensions = S.insert Ext_raw_tex pandocExtensions
                   , readerParseRaw = True
                   }

parseTeX :: String -> Either String LaTeX
parseTeX = Atto.parseOnly latexParser . T.pack

texToMarkdown :: String -> IO Pandoc
texToMarkdown src = do
  macros <- fst . parseMacroDefinitions <$> readFile "/Users/hiromi/Library/texmf/tex/platex/mystyle.sty"
  let rewritten = T.unpack $ render $ rewriteCommands $ view _Right $ parseTeX $ applyMacros macros $ src
      pandoc = rewriteEnv $ readLaTeX myReaderOpts rewritten
  return pandoc

rewriteCommands :: LaTeX -> LaTeX
rewriteCommands = bottomUp rewrite
  where
    expands = ["Set", "Braket"]
    rewrite (TeXComm "underline" [FixArg src]) = TeXRaw $ T.concat ["<u>", render src, "</u>"]
    rewrite (TeXComm comm [FixArg src]) | comm `elem` expands =
      case breakTeXOn "|" src of
        Just (lhs, rhs) -> TeXComm comm [FixArg lhs, FixArg rhs]
        Nothing -> TeXComm (T.unpack $ T.toLower $ T.pack comm) [FixArg src]
    rewrite t = t

breakTeXOn :: T.Text -> LaTeX -> Maybe (LaTeX, LaTeX)
breakTeXOn _ TeXEmpty = Nothing
breakTeXOn s (TeXRaw t) =
  case T.breakOn s t of
    (_, "") -> Nothing
    answer -> answer & _2 %~ T.drop 1
                     & both %~ TeXRaw
                     & Just
breakTeXOn s (TeXSeq l r) =
      do (l0, l1) <- breakTeXOn s l
         return (l0, TeXSeq l1 r)
  <|> do (r0, r1) <- breakTeXOn s r
         return (TeXSeq l r0, r1)
breakTeXOn _ _ = Nothing

envs :: [String]
envs = [ "prop", "proof", "theorem", "lemma", "axiom"
       , "definition", "question", "answer", "problem", "corollary"
       ]

commandDic :: [(String, Either ([Inline] -> Inline) String)]
commandDic = [("underline", Right "u"), ("bf", Left Strong)]

rewriteInlineCmd :: [Inline] -> [Inline]
rewriteInlineCmd = concatMap step
  where
    step (RawInline "latex" src)
        | Right t <- parseTeX src = rewrite t
    step i = [i]
    rewrite (TeXSeq l r) = rewrite l ++ rewrite r
    rewrite c@(TeXComm cmd [FixArg arg]) =
      case lookup cmd commandDic of
        Just (Right tag) -> [ RawInline "html" $ "<" ++ tag ++ ">"
                            , RawInline "latex" $ T.unpack $ render arg
                            , RawInline "html" $ "</" ++ tag ++ ">"
                            ]
        Just (Left inl) -> [inl [RawInline "latex" $ T.unpack $ render arg]]
        _ -> [RawInline "latex" $ T.unpack $render c]
    rewrite c = [RawInline "latex" $ T.unpack $ render c]

rewriteEnv :: Pandoc -> Pandoc
rewriteEnv (Pandoc meta bs) = Pandoc meta $ bottomUp rewriteInlineCmd $ rewriteBeginEnv bs

rewriteBeginEnv :: [Block] -> [Block]
rewriteBeginEnv = concatMap step
  where
    step (RawBlock "latex" src)
      | Right (TeXEnv env args body) <- parseTeX src
      , env `elem` envs =
          let divStart
                  | null args = concat ["<div class=\"", env, "\">"]
                  | otherwise = concat ["<div class=\"", env, "\" name=\""
                                       , toUnicode TextNormal $ unwords $ map (init . tail . T.unpack . render) args, "\">"]
              Pandoc _ myBody = rewriteEnv $ readLaTeX myReaderOpts $ T.unpack $ render body
          in RawBlock "html" divStart : myBody ++ [RawBlock "html" "</div>"]
    step b = [b]
