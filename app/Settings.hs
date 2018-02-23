{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving              #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE QuasiQuotes                                                 #-}
module Settings where

import           Control.Applicative      (empty, (<|>))
import           Control.Arrow            (first)
import           Data.Default             (Default (..))
import           Data.Foldable            (asum, toList)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Monoid              ((<>))
import           Data.Store               (Store)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Data.Yaml
import           GHC.Generics             (Generic)
import           Instances                ()
import qualified Text.Megaparsec          as MP
import qualified Text.Mustache            as Mus
import           Text.Mustache.Compile.TH (mustache)
import           Web.Sake

data SiteTree = SiteTree { title    :: T.Text
                         , children :: HashMap FilePath SiteTree
                         } deriving stock    (Show, Eq, Generic, Typeable)
                           deriving anyclass (Store)

instance Readable SiteTree where
  readFrom_ = defaultYamlReadFrom_

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
                     } deriving (Typeable, Read, Show, Eq, Ord, Generic)
                       deriving anyclass (ToJSON, FromJSON, Store)

instance Writable Scheme where
  writeTo_ = defaultYamlWriteTo_

newtype Schemes  = Schemes { getSchemes :: HashMap T.Text Scheme
                           } deriving (Typeable, Show, Eq, Generic)
                             deriving newtype (FromJSON, ToJSON, Store)

instance Default Schemes where
  def = Schemes HM.empty

instance Readable Schemes where
  readFrom_ = defaultYamlReadFrom_

newtype NavBar  = NavBar { runNavBar :: [(T.Text, String)] }
                  deriving (Typeable, Generic)
                  deriving newtype (Store)

instance Readable NavBar where
  readFrom_ = defaultYamlReadFrom_

instance FromJSON NavBar where
  parseJSON = withArray "nav array" $ \vs -> NavBar <$> mapM p (toList vs)
    where
      p (Object d) | [(k, v)] <- HM.toList d = (,) k <$> parseJSON v
      p _          = empty

instance Default NavBar where
  def = NavBar [("Home", "/")]

data Card = Card { template :: Mus.Template
                 , gather   :: Bool
                 } deriving (Show, Eq, Ord, Generic, Store)

instance FromJSON Card where
  parseJSON val = withText "Template String" (fmap (flip Card False) . fromMus) val
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
           deriving (Show, Eq, Generic, Store)

instance FromJSON Cards where
  parseJSON = withObject "Card dictionary" $ \dic0 -> do
    dic <- mapM parseJSON dic0
    d <- asum $ return <$> HM.lookup "*" dic
    return $ Cards (HM.delete "*" dic) d

instance Readable Cards where
  readFrom_ = defaultYamlReadFrom_

instance Default Cards where
  def = Cards mempty $ Card [mustache|//hatenablog-parts.com/embed?url={{url}}|] False
