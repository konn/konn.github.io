{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable, DerivingStrategies                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes         #-}
module Settings where

import           Control.Applicative      (empty, (<|>))
import           Control.Arrow            (first)
import           Data.Binary              (Binary (..))
import qualified Data.Binary              as B
import qualified Data.ByteString.Lazy     as LBS
import           Data.Default             (Default (..))
import           Data.Foldable            (asum, toList)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Data.Yaml
import qualified Data.Yaml                as Y
import           GHC.Generics             (Generic)
import           Hakyll                   (cached)
import           Hakyll                   (compile)
import           Hakyll                   (match)
import           Hakyll                   (getResourceLBS)
import           Hakyll                   (Rules)
import           Hakyll.Core.Writable     (Writable (..))
import           Instances                ()
import qualified Text.Megaparsec          as MP
import qualified Text.Mustache            as Mus
import           Text.Mustache.Compile.TH (mustache)

data SiteTree = SiteTree { title    :: T.Text
                         , children :: HashMap FilePath SiteTree
                         } deriving (Show, Eq, Generic, Typeable)
instance Binary SiteTree

instance Writable SiteTree where
  write fp = write fp . fmap B.encode

instance FromJSON SiteTree where
  parseJSON (String txt) = pure $ SiteTree txt HM.empty
  parseJSON (Object dic)
    | [(name, chs)] <- HM.toList dic = SiteTree <$> pure name <*> HM.unions <$> parseJSON chs
  parseJSON _ = empty

instance Default SiteTree where
  def = SiteTree "konn-san.com 建設予定地" HM.empty

walkTree :: [FilePath] -> SiteTree -> [(FilePath, T.Text)]
walkTree [] (SiteTree t _) = [("/", t)]
walkTree (x : xs) (SiteTree t chs) = ("/", t) :
  case HM.lookup (dropSlash x) chs of
    Nothing  -> []
    Just st' -> map (first (("/" <> dropSlash x) <> )) (walkTree xs st')
  where
    dropSlash = flip maybe T.unpack <*> T.stripSuffix "/" . T.pack

data Scheme = Scheme { prefix  :: T.Text
                     , postfix :: Maybe T.Text
                     } deriving (Typeable, Read, Show, Eq,
                                 Ord, Generic)

instance FromJSON Scheme
instance Binary Scheme
instance ToJSON Scheme
instance Writable Scheme where
  write fp = write fp . fmap B.encode

newtype Schemes  = Schemes { getSchemes :: HashMap T.Text Scheme
                           } deriving (Typeable, Show, Eq, Generic)
                             deriving newtype (FromJSON, ToJSON, Binary)

instance Default Schemes where
  def = Schemes HM.empty

instance Writable Schemes where
  write fp = write fp . fmap B.encode

setting :: (Writable a, FromJSON a, Binary a, Typeable a)
        => String -> a -> Rules ()
setting name d = match (fromString $ "config/" <> name <> ".yml") $ compile $ cached name $ do
  fmap (fromMaybe d . Y.decode . LBS.toStrict) <$> getResourceLBS

newtype NavBar  = NavBar { runNavBar :: [(T.Text, String)] }
                  deriving (Typeable, Generic)
                  deriving newtype (Binary)

instance Writable NavBar where
  write fp = write fp . fmap B.encode

instance FromJSON NavBar where
  parseJSON (Array vs) = NavBar <$> mapM p (toList vs)
    where
      p (Object d) | [(k, v)] <- HM.toList d = (,) k <$> parseJSON v
      p _          = empty
  parseJSON _ = empty

instance Default NavBar where
  def = NavBar [("Home", "/")]

data Card = Card { template :: Mus.Template
                 , gather   :: Bool
                 } deriving (Show, Eq, Ord, Generic, Binary)

instance FromJSON Card where
  parseJSON val = withText "Template String" (\v -> flip Card False <$> fromMus v) val
              <|> withObject "Template Dict"
                  (\obj -> Card <$> (fromMus =<< obj .: "template")
                                <*> obj .: "gather"
                  ) val
    where
      fromMus txt =
        either (fail . MP.parseErrorPretty' txt)
        return $ Mus.compileMustacheText "url" txt

data Cards = Cards { cardDic     :: HashMap T.Text Card
                   , defaultCard :: Card
                   }
           deriving (Show, Eq, Generic, Binary)

instance FromJSON Cards where
  parseJSON = withObject "Card dictionary" $ \dic0 -> do
    dic <- mapM parseJSON dic0
    d <- asum $ fmap return $ HM.lookup "*" dic
    return $ Cards (HM.delete "*" dic) d

instance Writable Cards where
  write fp = write fp . fmap B.encode

instance Default Cards where
  def = Cards mempty $ Card [mustache|//hatenablog-parts.com/embed?url={{url}}|] False
