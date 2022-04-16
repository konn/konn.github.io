{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Breadcrumb
  ( makeBreadcrumb,
    makeBreadcrumbM,
    SiteTree (..),
    Breadcrumb (..),
    breadcrumbContext,
  )
where

import Control.Applicative (empty)
import Control.Arrow (first)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.=),
  )
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Bifunctor as Bi
import Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Instances ()
import Text.Mustache (renderMustache)
import Web.Sake
  ( Context,
    MonadSake,
    Mustache,
    Readable,
    defaultYamlReadFrom_,
    dropDirectory1,
    field,
    loadTemplate,
    readFromYamlFile',
    splitDirectories,
    takeDirectory,
    takeFileName,
  )
import Web.Sake.Class (Readable (..))
import Web.Sake.Item (Item (..), itemPath, lookupMetadata)

data SiteTree = SiteTree
  { title :: T.Text
  , children :: HM.HashMap FilePath SiteTree
  }
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (Store)

instance Readable SiteTree where
  readFrom_ = defaultYamlReadFrom_

instance FromJSON SiteTree where
  parseJSON (String txt) = pure $ SiteTree txt HM.empty
  parseJSON (Object dic)
    | [(name, chs)] <- KM.toList dic = SiteTree <$> pure (AK.toText name) <*> (HM.unions . map (HM.fromList . map (Bi.first T.unpack) . HM.toList . KM.toHashMapText)) <$> parseJSON chs
  parseJSON _ = empty

instance Default SiteTree where
  def = SiteTree "konn-san.com 建設予定地" HM.empty

walkTree :: [FilePath] -> SiteTree -> [(FilePath, T.Text)]
walkTree [] (SiteTree t _) = [("/", t)]
walkTree (x : xs) (SiteTree t chs) =
  ("/", t) :
  case HM.lookup (dropSlash x) chs of
    Nothing -> []
    Just st' -> map (first (("/" <> dropSlash x) <>)) (walkTree xs st')
  where
    dropSlash = flip maybe T.unpack <*> T.stripSuffix "/" . T.pack

makeBreadcrumb :: SiteTree -> Mustache -> Item Text -> Text
makeBreadcrumb st tmpl item =
  let path = itemPath item
      Just mytitle = lookupMetadata "title" item
      dropIndex fp
        | takeFileName fp == "index.md" = takeDirectory fp
        | otherwise = fp
      pars = splitDirectories $ dropIndex $ dropDirectory1 path
      bc
        | path == "index.md" = []
        | otherwise = walkTree pars st
      obj = toJSON $ Breadcrumb bc mytitle
   in LT.toStrict $ renderMustache tmpl obj

makeBreadcrumbM :: MonadSake m => Item Text -> m Text
makeBreadcrumbM item = do
  st <- readFromYamlFile' "config/tree.yml"
  src <- itemBody <$> loadTemplate "templates/breadcrumb.mustache"
  return $ makeBreadcrumb st src item

data Breadcrumb = Breadcrumb
  { parents :: [(String, T.Text)]
  , currentTitle :: String
  }
  deriving (Show, Eq, Ord)

instance ToJSON Breadcrumb where
  toJSON (Breadcrumb cbs ctr) =
    object
      [ "breadcrumbs" .= [object ["path" .= fp, "name" .= name] | (fp, name) <- cbs]
      , "currentTitle" .= ctr
      ]

breadcrumbContext :: String -> Context Text
breadcrumbContext key = field key makeBreadcrumbM
