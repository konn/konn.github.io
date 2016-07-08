{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances (biblioToBibTeX, bibTeXToBiblio, BibTeX(..)) where
import           Control.Applicative      ((<$>))
import           Data.Binary
import           Data.Data
import           Data.DeriveTH
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Hakyll.Core.Writable
import           Hakyll.Web.Pandoc.Biblio
import           Text.CSL                 hiding (Citation, Cite)

newtype BibTeX = BibTeX { runBibTeX :: [Reference] }
    deriving (Read, Show, Eq, Typeable)

instance Writable BibTeX where
  write _ _ = return ()

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

derive makeBinary ''BibTeX

biblioToBibTeX :: Biblio -> BibTeX
biblioToBibTeX (Biblio rs) = BibTeX rs

bibTeXToBiblio :: BibTeX -> Biblio
bibTeXToBiblio = Biblio . runBibTeX
