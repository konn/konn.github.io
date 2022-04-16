{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Instances (BibTeX (..)) where

import Citeproc hiding (Citation, Cite)
import Citeproc.Style
import Citeproc.Types
import Control.Arrow ((&&&), (>>>))
import Control.Exception
import Control.Lens hiding (from, to)
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Coerce
import Data.Data
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import Data.Store (Store (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.CrossRef ()
import qualified Text.Megaparsec as MegaParsec
import qualified Text.Mustache as Mus
import Text.Pandoc (readBibLaTeX)
import qualified Text.Pandoc as Pan
import Text.Pandoc.Builder (Inlines, Many (..))
import Text.Pandoc.Citeproc
import qualified Text.Pandoc.Definition as Pan
import Web.Sake

deriving anyclass instance Store Pan.QuoteType

deriving anyclass instance Store Pan.CitationMode

deriving anyclass instance Store Pan.Citation

deriving anyclass instance Store Pan.MathType

deriving anyclass instance Store Pan.Format

deriving anyclass instance Store Pan.ListNumberStyle

deriving anyclass instance Store Pan.ListNumberDelim

deriving anyclass instance Store Pan.Alignment

deriving anyclass instance Store Pan.Caption

deriving anyclass instance Store Pan.Block

deriving anyclass instance Store Pan.TableFoot

deriving anyclass instance Store Pan.RowHeadColumns

deriving anyclass instance Store Pan.TableBody

deriving anyclass instance Store Pan.ColWidth

deriving anyclass instance Store Pan.TableHead

deriving anyclass instance Store Pan.Row

deriving anyclass instance Store Pan.Cell

deriving anyclass instance Store Pan.ColSpan

deriving anyclass instance Store Pan.RowSpan

deriving anyclass instance Store Pan.Inline

newtype BibTeX = BibTeX {runBibTeX :: [Reference Inlines]}
  deriving (Show, Typeable, Generic)
  deriving anyclass (Store)

instance Writable BibTeX where
  writeTo_ fp = writeTo_ fp . Binary

deriving anyclass instance Typeable Measure

deriving anyclass instance Store Measure

deriving anyclass instance Typeable TeXArg

deriving anyclass instance Store TeXArg

deriving anyclass instance Typeable MathType

deriving anyclass instance Store MathType

deriving anyclass instance Store LaTeX

deriving anyclass instance Plated LaTeX

deriving instance Generic (Reference a)

deriving anyclass instance Store a => Store (Reference a)

newtype GVariable = GVariable {runGVariable :: Variable}

instance Generic GVariable where
  type
    Rep GVariable =
      D1
        ( 'MetaData "GVariable" "Instances" "site" 'True)
        ( C1
            ( 'MetaCons "GVariable'" 'PrefixI 'False)
            ( S1
                ( 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 T.Text)
            )
        )
  to = GVariable . toVariable . coerce
  from = coerce . fromVariable . runGVariable

deriving anyclass instance Store GVariable

deriving via GVariable instance Store Variable

deriving instance Generic (Val a)

deriving anyclass instance Store a => Store (Val a)

deriving anyclass instance Store a => Store (Many a)

deriving instance Generic ItemId

deriving anyclass instance Store ItemId

deriving instance Generic NameHints

deriving anyclass instance Store NameHints

deriving instance Generic Name

deriving anyclass instance Store Name

deriving instance Generic DateParts

deriving newtype instance Store DateParts

deriving instance Generic Date

deriving anyclass instance Store Date

deriving instance Generic DisambiguationData

deriving anyclass instance Store DisambiguationData

instance FromJSON LaTeX where
  parseJSON = withText "Text" $ either (fail . show) return . parseLaTeX

instance Store MegaParsec.Pos where
  peek = fmap MegaParsec.mkPos peek
  poke = poke . MegaParsec.unPos
  size = contramap MegaParsec.unPos size

instance Store Mus.Key

instance Store Mus.PName

instance Store Mus.Node

instance Store Mus.Template

instance Writable Mus.Template where
  writeTo_ fp = writeTo_ fp . Binary

instance {-# OVERLAPPING #-} Readable (Style Inlines) where
  readFrom_ = either (throwIO . userError . show) pure <=< parseStyle pure <=< T.readFile

newtype WrapKeyMap a = WrapKeyMap {runWrapKeyMap :: KM.KeyMap a}

instance Generic (WrapKeyMap a) where
  type
    Rep (WrapKeyMap a) =
      D1
        ( 'MetaData "WrapKeyMap" "Instances" "site" 'True)
        ( C1
            ( 'MetaCons "WrappedKeyMap" 'PrefixI 'False)
            ( S1
                ( 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 (HM.HashMap T.Text a))
            )
        )
  to = WrapKeyMap . KM.fromHashMapText . coerce
  from = coerce . KM.toHashMapText . runWrapKeyMap

deriving anyclass instance (Store a) => Store (WrapKeyMap a)

newtype GScientific = GScientific {runGScientific :: Scientific}

instance Generic GScientific where
  type
    Rep GScientific =
      D1
        ( 'MetaData "GScientific" "Instances" "site" 'True)
        ( C1
            ( 'MetaCons "GScientific'" 'PrefixI 'False)
            ( S1
                ( 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 Integer)
            )
            :*: ( S1
                    ( 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (Rec0 Int)
                )
        )
  to =
    coerce >>> \case
      K1 l :*: K1 r -> GScientific $ scientific l r
  from = runGScientific >>> (coefficient &&& base10Exponent) >>> from >>> coerce

deriving anyclass instance Store GScientific

deriving via GScientific instance Store Scientific

deriving anyclass instance Store Value

deriving via (WrapKeyMap a) instance (Store a) => Store (KM.KeyMap a)
