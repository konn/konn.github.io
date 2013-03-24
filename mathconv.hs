{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternGuards, TemplateHaskell                             #-}
module MathConv (texToMarkdown) where
import qualified Data.Attoparsec.Text   as Atto
import           Data.Default
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Text.LaTeX.Base
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           Text.Pandoc

myReaderOpts :: ReaderOptions
myReaderOpts = def { readerExtensions = S.insert Ext_raw_tex pandocExtensions
                   , readerParseRaw = True
                   }

texToMarkdown :: String -> Pandoc
texToMarkdown src =
  let pandoc = rewriteEnv $ readLaTeX myReaderOpts src
  in pandoc

envs :: [String]
envs = [ "prop", "proof", "theorem", "lemma", "axiom"
       , "definition", "question", "answer"
       ]

rewriteEnv :: Pandoc -> Pandoc
rewriteEnv (Pandoc meta bs) = Pandoc meta $ rewriteBeginEnv bs

rewriteBeginEnv :: [Block] -> [Block]
rewriteBeginEnv = concatMap step
  where
    step (RawBlock "latex" src)
      | Right (TeXEnv env args body) <- Atto.parseOnly latexParser (T.pack src)
      , env `elem` envs =
          let divStart
                  | null args = concat ["<div class=\"", env, "\">"]
                  | otherwise = concat ["<div class=\"", env, "\" name=\""
                                       , unwords $ map (init . tail . T.unpack . render) args, "\">"]
              Pandoc _ myBody = rewriteEnv $ readLaTeX myReaderOpts $ T.unpack $ render body
          in RawBlock "html" divStart : myBody ++ [RawBlock "html" $ concat ["</div>"]]
    step b = [b]
