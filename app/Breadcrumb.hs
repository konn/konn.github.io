{-# LANGUAGE OverloadedStrings #-}
module Breadcrumb (makeBreadcrumb, Breadcrumb(..), breadcrumbContext) where
import Instances ()
import Settings
import Utils

import           Data.Aeson          (ToJSON (..), object, (.=))
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Text.Mustache       (renderMustache)
import           Web.Sake            (Context, MonadSake, field, loadTemplate,
                                      readFromYamlFile', runIdentifier,
                                      splitDirectories, takeDirectory,
                                      takeFileName)



import Web.Sake.Item (Item (..))

makeBreadcrumb :: MonadSake m => Item Text -> m Text
makeBreadcrumb item = do
  let ident = itemIdentifier item
      Just mytitle = fromJSON' =<< HM.lookup "title" (itemMetadata item)
  st <- readFromYamlFile' "config/tree.yml"
  let dropIndex fp | takeFileName fp == "index.html" = takeDirectory fp
                   | otherwise = fp
      pars = splitDirectories $ dropIndex $ runIdentifier ident
      bc | ident == "index.md" = []
         | otherwise = walkTree pars st
  src <- itemBody <$> loadTemplate "templates/breadcrumb.mustache"
  let obj = toJSON $ Breadcrumb bc mytitle
  return $ LT.toStrict $ renderMustache src obj


data Breadcrumb = Breadcrumb { parents      :: [(String, T.Text)]
                             , currentTitle :: String
                             }
                deriving (Show, Eq, Ord)

instance ToJSON Breadcrumb where
  toJSON (Breadcrumb cbs ctr) =
    object ["breadcrumbs" .= [object ["path" .= fp, "name" .= name] | (fp, name) <- cbs ]
             ,"currentTitle" .= ctr
             ]

breadcrumbContext :: String -> Context Text
breadcrumbContext key = field key makeBreadcrumb
