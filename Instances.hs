{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances (biblioToBibTeX, bibTeXToBiblio, BibTeX(..)) where
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

deriving instance Typeable Measure
deriving instance Binary Measure
deriving instance Typeable TeXArg
deriving instance Binary TeXArg
deriving instance Typeable MathType
deriving instance Binary MathType
deriving instance Binary LaTeX

deriving instance Plated LaTeX

instance FromJSON LaTeX where
  parseJSON = withText "Text" $ either (fail . show) return . parseLaTeX

