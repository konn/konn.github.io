{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards, StandaloneDeriving      #-}
{-# LANGUAGE TypeSynonymInstances, ViewPatterns                    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro where
import Instances ()
import Lenses

import           Control.Applicative
import           Control.Lens           ((^?!))
import           Control.Lens.Extras
import           Control.Lens.Plated
import           Control.Monad          (guard)
import           Control.Monad.Writer
import           Data.Binary            (Binary (..))
import qualified Data.Binary            as B
import           Data.Char
import           Data.Data
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.List              as L
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Data.Yaml.Aeson
import           GHC.Generics
import           Hakyll.Core.Writable
import           Text.LaTeX             (render)
import           Text.LaTeX.Base.Syntax
import           Text.Regex.Applicative

data TeXMacro = TeXMacro { macroArgs    :: Int
                         , macroBody    :: LaTeX
                         , macroOptArgs :: [LaTeX]
                         }
              deriving (Data, Typeable, Show, Eq, Generic, Binary)

instance FromJSON TeXMacro where
  parseJSON (String t) = TeXMacro 0 <$> parseJSON (String t)
                                    <*> pure []
  parseJSON (Array v)  =
    TeXMacro <$> maybe empty parseJSON (v V.!? 0)
             <*> maybe empty
                       parseJSON
                       (v V.!? 1)
             <*> mapM parseJSON (drop 2 $ V.toList v)
  parseJSON (Object dic) =
    TeXMacro <$> dic .: "argNums"
             <*> dic .: "body"
             <*> dic .: "optArgs"
  parseJSON _ = empty

type TeXMacros = HashMap String TeXMacro

instance Writable TeXMacros where
  write fp = write fp . fmap B.encode


flattenOptArgs :: TeXArg -> Maybe LaTeX
flattenOptArgs (OptArg l)   = Just l
flattenOptArgs (MOptArg ls) = Just $ foldl1 TeXSeq $  L.intersperse "," ls
flattenOptArgs _            = Nothing

pattern AnyOptArg :: LaTeX -> TeXArg
pattern AnyOptArg lat <- (flattenOptArgs -> Just lat) where
  AnyOptArg lat = OptArg lat

applyTeXMacro :: TeXMacros -> LaTeX -> LaTeX
applyTeXMacro dic = rewrite matcher
  where
    matchOptArgs [] margs = ([], margs)
    matchOptArgs (_ : ds) (AnyOptArg lat : ars) =
      let (ropts, rargs) = matchOptArgs ds ars
      in (lat : ropts, rargs)
    matchOptArgs ds args = (ds, args)

    matcher (TeXCommS cmd) = matcher (TeXComm cmd [])
    matcher (TeXComm cmd margs) = do
      TeXMacro{..} <- HM.lookup cmd dic
      let optCount = length macroOptArgs
          fixCount = macroArgs - optCount
          (opts, sargs) = matchOptArgs macroOptArgs margs
          (firsts, rest0) = splitAt fixCount sargs
          (fixs, rest1) = span (is _FixArg) firsts
          args = opts ++ map (^?! _FixArg) fixs
          rest = rest1 ++ rest0
      guard $ length args >= macroArgs
      return $ foldl TeXSeq (transform (apply args) macroBody) $
               map formatArg rest
    matcher _ = Nothing

    apply args (TeXRaw src) =
      fromMaybe (TeXRaw src) $
      match (reFoldl Greedy (\a b -> TeXSeq a b) TeXEmpty $ pat args) $
      T.unpack src
    apply _ t = t

    pat args = sym '#' *> (chk . pred . read <$> some (psym isAsciiDigit)
                       <|> TeXRaw . T.pack <$> many (psym (/= '#')))
           <|> TeXRaw . T.pack <$> many (psym (/= '#'))
      where chk i = if i < len then args !! i else TeXRaw $ "#" <> T.pack (show i)
            len = length args

formatArg :: TeXArg -> LaTeX
formatArg (AnyOptArg l) = foldr1 TeXSeq ["[", l, "]"]
formatArg (FixArg l)    = TeXBraces l
formatArg (SymArg l)    = foldr1 TeXSeq ["<", l, ">"]
formatArg (ParArg l)    = foldr1 TeXSeq ["<", l, ">"]
formatArg (MSymArg ls)  = foldr1 TeXSeq $ "<" : ls ++  [">"]
formatArg (MParArg ls)  = foldr1 TeXSeq $ "<" : ls ++  [">"]
formatArg a             = TeXRaw $ render a

isAsciiDigit :: Char -> Bool
isAsciiDigit c = isDigit c && isAscii c

parseTeXMacro :: LaTeX -> Maybe (String, TeXMacro)
parseTeXMacro (TeXComm ncmd ars)
  | isNewCommand ncmd
  , (FixArg (TeXCommS name) : opts, [FixArg body]) <- splitAt (length ars - 1) ars
  , all (\a -> is _OptArg a || is _MOptArg a) opts
  = case opts of
      [] -> Just (name, TeXMacro 0 body [])
      (OptArg (TeXRaw nstr) : os)
        | [(n, "")] <- reads (T.unpack nstr) ->
          Just (name, TeXMacro n body $ map fromOptArg os)
      _ -> Nothing
  | otherwise = Nothing
  where
    fromOptArg (MOptArg ls) = foldr1 TeXSeq $ L.intersperse (TeXRaw ",") ls
    fromOptArg ~(OptArg l)  = l
parseTeXMacro _ = Nothing

isNewCommand :: String -> Bool
isNewCommand ncmd =
  ncmd `elem` [pre ++ "newcommand" ++ post | pre <- ["", "re"], post <- ["", "*"]]

parseTeXMacros :: [LaTeX] -> TeXMacros
parseTeXMacros = HM.fromList . mapMaybe parseTeXMacro
