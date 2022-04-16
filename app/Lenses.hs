{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lenses where

import Control.Lens
import Data.Text (Text)
import Language.Haskell.TH hiding (Inline)
import Text.HTML.TagSoup
import Text.LaTeX.Base.Syntax
import Text.Pandoc.Definition
import Web.Sake.Conf

flip makeLensesWith ''SakeConf $
  underscoreFields & lensField .~ \_ _ name -> [TopName $ mkName $ '_' : nameBase name]

newtype MonoidFun a w = MonoidFun {runMonoidArr :: a -> w}
  deriving (Semigroup, Monoid, Functor)

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

linkUrl :: Traversal' Inline Text
linkUrl = imgOrLink . _1 . _1
