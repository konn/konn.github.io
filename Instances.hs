{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances (biblioToBibTeX, bibTeXToBiblio, BibTeX(..)) where
import Data.Binary
import Data.Data
import Data.DeriveTH
import Hakyll.Core.Writable
import Hakyll.Web.Pandoc.Biblio
import Text.CSL                 hiding (Citation, Cite)
import Text.CSL.Reference
import Text.CSL.Style           hiding (Citation, Cite)
import Text.Pandoc.Definition

newtype BibTeX = BibTeX { runBibTeX :: [Reference] }
    deriving (Read, Show, Eq, Typeable)

instance Writable BibTeX where
  write _ _ = return ()

derive makeBinary ''Inline
derive makeBinary ''Block
derive makeBinary ''Alignment
derive makeBinary ''Format
derive makeBinary ''ListNumberDelim
derive makeBinary ''ListNumberStyle
derive makeBinary ''MathType
derive makeBinary ''Citation
derive makeBinary ''CitationMode
derive makeBinary ''QuoteType
derive makeBinary ''Formatted
derive makeBinary ''Literal
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
