{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PubTalks where

import Control.Applicative
import Control.Lens
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Store
import Data.Text (Text)
import GHC.Generics

data Talk = Talk
  { title :: Text
  , conference :: Conference
  , date :: Text
  , link :: Maybe Text
  , links :: Maybe [LinkItem]
  }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Store)

data LinkItem = LinkItem {linkItemName :: Text, linkItemUrl :: Text}
  deriving (Read, Show, Eq, Ord, Generic, Store)

liOpts :: Options
liOpts = defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

instance ToJSON LinkItem where
  toJSON = genericToJSON liOpts

instance {-# OVERLAPPING #-} FromJSON [LinkItem] where
  parseJSON obj =
    withObject "Dictionary" parsePairs obj
      <|> withArray "Dictionary" (fmap toList . mapM parsePair) obj
      <|> withArray "Dictionary" (fmap toList . mapM (genericParseJSON liOpts)) obj
    where
      parsePairs hm = do
        dic <- mapM parseJSON hm
        return $ map (uncurry LinkItem) $ HM.toList $ KM.toHashMapText dic
      parsePair hm = do
        [(l, t)] <- HM.toList . KM.toHashMapText <$> parseJSON hm
        return $ LinkItem l t

data Conference = Conference
  { cnfName :: Text
  , cnfUrl :: Maybe Text
  , cnfVenue :: Maybe Text
  }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving (Store)

cnfOption :: Options
cnfOption =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    , omitNothingFields = True
    , unwrapUnaryRecords = True
    }

instance ToJSON Conference where
  toJSON = genericToJSON cnfOption
  {-# INLINE toJSON #-}

instance FromJSON Conference where
  parseJSON j =
    withText
      "Conference name expected"
      (\t -> pure $ Conference t Nothing Nothing)
      j
      <|> genericParseJSON cnfOption j

addItemInfo :: Talk -> Value
addItemInfo talk@Talk {..} =
  let Object dic = toJSON talk
   in Object $
        KM.insert "hasLinks" (Bool $ not $ null links) $
          KM.insert
            "links"
            ( toJSON $
                links <&> \ls ->
                  imap (addLinkInfo (length ls)) ls
            )
            $ KM.delete "links" dic

addLinkInfo :: Int -> Int -> LinkItem -> Value
addLinkInfo maxLen pos li =
  let Object dic = toJSON li
   in toJSON $ KM.insert "notLast" (Bool $ pos < maxLen - 1) dic
