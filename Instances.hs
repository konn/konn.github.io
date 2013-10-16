{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances (biblioToBibTeX, bibTeXToBiblio, BibTeX(..)) where
import Data.Binary
import Data.Data
import Data.DeriveTH
import Hakyll.Core.Writable
import Hakyll.Web.Pandoc.Biblio
import Text.CSL
import Text.CSL.Reference

newtype BibTeX = BibTeX { runBibTeX :: [Reference] }
    deriving (Read, Show, Eq, Typeable)

instance Writable BibTeX where
  write _ _ = return ()

derive makeBinary ''Agent
derive makeBinary ''RefDate
derive makeBinary ''RefType
derive makeBinary ''CNum
derive makeBinary ''Reference
derive makeBinary ''BibTeX

biblioToBibTeX :: Biblio -> BibTeX
biblioToBibTeX (Biblio rs) = BibTeX rs

bibTeXToBiblio :: BibTeX -> Biblio
bibTeXToBiblio = Biblio . runBibTeX
