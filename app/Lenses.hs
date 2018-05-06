{-# LANGUAGE DeriveFunctor, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}
{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances       #-}
module Lenses where
import Control.Lens
import Language.Haskell.TH    hiding (Inline)
import Text.HTML.TagSoup
import Text.LaTeX.Base.Syntax
import Text.Pandoc.Definition
import Web.Sake.Conf

flip makeLensesWith ''SakeConf $
  underscoreFields & lensField .~ \_ _ name -> [TopName $ mkName $ '_' : nameBase name]

newtype MonoidFun a w = MonoidFun { runMonoidArr :: a -> w }
                        deriving (Monoid, Functor)

makePrisms ''MonoidFun
makeWrapped ''MonoidFun
makePrisms ''Inline
makePrisms ''Pandoc
makePrisms ''Block
makePrisms ''MetaValue
makeFields ''Inline
makeFields ''Block
makePrisms ''LaTeX
makePrisms ''TeXArg
makePrisms ''Tag

imgOrLink :: Traversal' Inline (Attr, [Inline], Target)
imgOrLink = failing _Link _Image

linkUrl :: Traversal' Inline String
linkUrl = imgOrLink . _1 . _1
