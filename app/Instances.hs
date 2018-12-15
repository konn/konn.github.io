{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, StandaloneDeriving             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances (BibTeX(..)) where
import           Control.Lens
import           Data.Aeson
import           Data.Data
import           Data.Store             (Store (..))
import           GHC.Generics
import           Text.CSL               hiding (Citation, Cite)
import           Text.CSL.Reference
import           Text.CSL.Style
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.CrossRef    ()
import qualified Text.Megaparsec        as MegaParsec
import qualified Text.Mustache          as Mus
import qualified Text.Pandoc.Definition as Pan

import Web.Sake

deriving instance Store Pan.QuoteType
deriving instance Store Pan.CitationMode
deriving instance Store Pan.Citation
deriving instance Store Pan.MathType
deriving instance Store Pan.Format
deriving instance Store Pan.ListNumberStyle
deriving instance Store Pan.ListNumberDelim
deriving instance Store Pan.Alignment
deriving instance Store Pan.Block
deriving instance Store Pan.Inline

deriving instance Store RefType
deriving instance Store Literal
deriving instance Store Formatted
deriving instance Store Agent
deriving instance Store Season
deriving instance Store RefDate
deriving instance Store CNum
deriving instance Store Reference
deriving instance Store CLabel

newtype BibTeX = BibTeX { runBibTeX :: [Reference] }
    deriving (Read, Show, Eq, Typeable, Generic, Store)

instance Writable BibTeX where
  writeTo_ fp = writeTo_ fp . Binary

deriving instance Typeable Measure
deriving instance Store Measure
deriving instance Typeable TeXArg
deriving instance Store TeXArg
deriving instance Typeable MathType
deriving instance Store MathType
deriving instance Store LaTeX

deriving instance Plated LaTeX

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

instance {-# OVERLAPPING #-} Readable Style where
  readFrom_ = readCSLFile Nothing

instance {-# OVERLAPPING #-} Readable [Reference] where
  readFrom_ = readBiblioFile (const True)
