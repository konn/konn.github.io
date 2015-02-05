{-# LANGUAGE DeriveFunctor, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}
{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeSynonymInstances       #-}
module Lenses where
import Control.Lens
import Data.Monoid
import Hakyll.Core.Configuration
import Language.Haskell.TH       hiding (Inline (..))
import Text.Pandoc.Definition

flip makeLensesWith ''Configuration $
  underscoreFields & lensField .~ \_ _ name -> [TopName $ mkName $ '_' : nameBase name]

newtype MonoidFun a w = MonoidFun { runMonoidArr :: a -> w }
                        deriving (Monoid, Functor)

makePrisms ''MonoidFun
makeWrapped ''MonoidFun
makePrisms ''Inline
makePrisms ''Block
makeFields ''Inline
makeFields ''Block

imgOrLink :: Traversal' Inline ([Inline], Target)
imgOrLink = failing _Link _Image

linkUrl :: Traversal' Inline String
linkUrl = imgOrLink . _2 . _1

