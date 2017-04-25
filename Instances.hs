{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances (biblioToBibTeX, bibTeXToBiblio, BibTeX(..)) where
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Binary
import           Data.Data
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           GHC.Generics
import           Hakyll.Core.Writable
import           Hakyll.Web.Pandoc.Biblio
import           Text.CSL                 hiding (Citation, Cite)
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.CrossRef      ()

newtype BibTeX = BibTeX { runBibTeX :: [Reference] }
    deriving (Read, Show, Eq, Typeable, Generic)

instance Writable BibTeX where
  write _ _ = return ()

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

deriving instance Binary BibTeX

biblioToBibTeX :: Biblio -> BibTeX
biblioToBibTeX (Biblio rs) = BibTeX rs

bibTeXToBiblio :: BibTeX -> Biblio
bibTeXToBiblio = Biblio . runBibTeX

deriving instance Generic  Measure
deriving instance Typeable Measure
deriving instance Data Measure
deriving instance Binary Measure
deriving instance Generic TeXArg
deriving instance Typeable TeXArg
deriving instance Data TeXArg
deriving instance Binary TeXArg
deriving instance Generic  MathType
deriving instance Typeable MathType
deriving instance Data MathType
deriving instance Binary MathType
deriving instance Data LaTeX
deriving instance Binary LaTeX

deriving instance Plated LaTeX

instance FromJSON LaTeX where
  parseJSON = withText "Text" $ either (const empty) return . parseLaTeX

