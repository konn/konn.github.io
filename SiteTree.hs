{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, ViewPatterns       #-}
module SiteTree where

import           Control.Applicative  (empty)
import           Control.Applicative  (pure, (<$>), (<*>))
import           Control.Arrow        (first)
import           Data.Binary          (Binary (..))
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           Data.Yaml
import           GHC.Generics         (Generic)
import           Hakyll.Core.Writable (Writable (..))

import Data.Typeable (Typeable)
import Instances     ()

data SiteTree = SiteTree { title    :: T.Text
                         , children :: HashMap FilePath SiteTree
                         } deriving (Show, Eq, Generic, Typeable)
instance Binary SiteTree

instance Writable SiteTree where
  write _ _ = return ()

instance FromJSON SiteTree where
  parseJSON (String txt) = pure $ SiteTree txt HM.empty
  parseJSON (Object dic)
    | [(name, chs)] <- HM.toList dic = SiteTree <$> pure name <*> HM.unions <$> parseJSON chs
  parseJSON _ = empty

walkTree :: [FilePath] -> SiteTree -> [(FilePath, T.Text)]
walkTree [] (SiteTree t _) = [("/", t)]
walkTree (x : xs) (SiteTree t chs) = ("/", t) :
  case HM.lookup x chs of
    Nothing -> []
    Just st' -> map (first (("/" <> x) <> )) (walkTree xs st')
