{-# LANGUAGE DeriveFunctor, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies        #-}
{-# LANGUAGE TypeSynonymInstances                                     #-}
module Lenses where
import Control.Lens
import Data.Monoid
import Hakyll.Core.Configuration
import Language.Haskell.TH

flip makeLensesWith ''Configuration $
  underscoreFields & lensField .~ \_ _ name -> [TopName $ mkName $ '_' : nameBase name]


newtype MonoidFun a w = MonoidFun { runMonoidArr :: a -> w }
                        deriving (Monoid, Functor)

makePrisms ''MonoidFun
makeWrapped ''MonoidFun
