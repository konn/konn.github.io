{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Macro where
import Instances ()
import Lenses

import           Control.Applicative
import           Control.Lens           ((^?!))
import           Control.Lens.Extras
import           Control.Lens.Plated
import           Control.Monad          (guard)
import           Data.Binary            (Binary (..))
import qualified Data.Binary            as B
import           Data.Char
import           Data.Data
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Data.Yaml.Aeson
import           GHC.Generics
import           Hakyll.Core.Writable
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
  parseJSON _ = empty

type TeXMacros = HashMap String TeXMacro

instance Writable TeXMacros where
  write fp = write fp . fmap B.encode


applyTeXMacro :: TeXMacros -> LaTeX -> LaTeX
applyTeXMacro dic = rewrite matcher
  where
    matcher (TeXCommS cmd) = matcher (TeXComm cmd [])
    matcher (TeXComm cmd margs) = do
      TeXMacro{..} <- HM.lookup cmd dic
      let (opts, sargs) = span (is _OptArg) margs
      guard $ length opts <= length macroOptArgs
      let bracs = concat [ map (^?! _OptArg) opts
                         , drop (length opts) macroOptArgs
                         , map (^?! _FixArg) sargs
                         ]
          (args, bs) = splitAt macroArgs bracs
      return $ foldl TeXSeq (transform (apply args) macroBody) $
               map TeXBraces bs
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

isAsciiDigit :: Char -> Bool
isAsciiDigit c = isDigit c && isAscii c
