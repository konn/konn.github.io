{-# LANGUAGE ExistentialQuantification, GADTs, NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                                                  #-}
module MustacheTemplate
       ( MusContext (), applyMustacheTemplate, loadAndApplyMustache
       , applyAsMustache
       , metadataField, field, constField, bodyField, urlField
       , pathField, titleField, dateFieldWith, dateField
       , defaultMusContext, modificationTimeFieldWith, modificationTimeField
       , teaserField, teaserFieldWithSeparator, boolField
       , itemsFieldWithContext', itemsFieldWithContext, itemsField
       )
       where
import Instances ()

import           Control.Monad.Except        (throwError)
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy           as HM
import           Data.List.NonEmpty
import           Data.Semigroup
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import           Data.Time
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Util.String     (needlePrefix)
import           Hakyll.Web.Html
import           Hakyll.Web.Template.Context (getItemModificationTime,
                                              getItemUTC)
import           System.FilePath             (takeBaseName)
import           Text.Megaparsec             (parseErrorPretty)
import           Text.Mustache

data MusContext a = SimpleContext Metadata
                  | MusContext (Item a -> Compiler Metadata)

instance Semigroup (MusContext a) where
  SimpleContext val <> SimpleContext val' = SimpleContext $ val <> val'
  MusContext get <> SimpleContext val' = MusContext $ \i -> do
    val <- get i
    return $ val <> val'
  SimpleContext val <> MusContext get' = MusContext $ \i -> do
    val' <- get' i
    return $ val <> val'
  MusContext get <> MusContext get' = MusContext $ \i -> do
    (<>) <$> get i <*> get' i


instance Monoid (MusContext a) where
  mappend = (<>)
  mempty  = SimpleContext mempty


metadataField :: MusContext a
metadataField = MusContext $ \item ->
  getMetadata (itemIdentifier item)

field :: ToJSON a => String -> (Item b -> Compiler a) -> MusContext b
field k mk = MusContext $ \i -> do
  debugCompiler $ "Generating field " ++ show k
  v <- mk i
  debugCompiler $ "Generated field " ++ show (k, toJSON v) ++ "."
  return $ HM.singleton (T.pack k) $ toJSON v

constField :: ToJSON a => String -> a -> MusContext b
constField k v = SimpleContext $ HM.singleton (T.pack k) $ toJSON v

bodyField :: ToJSON a => String -> MusContext a
bodyField key = field key $ return . itemBody

pathField :: String -> MusContext a
pathField key = field key $ return . toFilePath . itemIdentifier

urlField :: String -> MusContext a
urlField key = field key $
    fmap (maybe mempty toUrl) . getRoute . itemIdentifier

defaultMusContext :: ToJSON a => MusContext a
defaultMusContext =
  sconcat  $
     bodyField "body"
  :| [ metadataField
     , urlField "url"
     , pathField "path"
     , titleField "title"
     ]

titleField :: String -> MusContext a
titleField key = field key $ return . takeBaseName . toFilePath . itemIdentifier

dateField :: String -> String -> MusContext a
dateField = dateFieldWith defaultTimeLocale

dateFieldWith :: TimeLocale     -- ^ Output time locale
              -> String         -- ^ Destination key
              -> String         -- ^ Format to use on the date
              -> MusContext a   -- ^ Resulting context
dateFieldWith locale key format = field key $ \i -> do
    time <- getItemUTC locale $ itemIdentifier i
    return $ formatTime locale format time

applyAsMustache :: MusContext String -> Item String -> Compiler (Item String)
applyAsMustache ctx item =
  let fname = fromString $ toFilePath $ itemIdentifier item
  in case compileMustacheText fname $ T.pack $ itemBody item of
    Left err  -> throwError $ lines $ parseErrorPretty err
    Right tpl -> applyMustacheTemplate tpl ctx item

applyMustacheTemplate :: Template -> MusContext a -> Item a -> Compiler (Item String)
applyMustacheTemplate tmpl (SimpleContext val) i = do
  debugCompiler $
    concat [ "\tApplying template "
           , show tmpl
           , " to a simple context "
           , show val]
  return $ itemSetBody (LT.unpack $ renderMustache tmpl $ toJSON val) i
applyMustacheTemplate tmpl (MusContext f) i = do
  debugCompiler $
    concat [ "\tApplying template "
           , show tmpl
           ]
  val <- f i
  debugCompiler $ concat [ "\t\t... to a complex context "
                         , show val]
  return $ itemSetBody (LT.unpack $ renderMustache tmpl $ toJSON val) i

loadAndApplyMustache :: Identifier -> MusContext a -> Item a -> Compiler (Item String)
loadAndApplyMustache ident ctx item = do
  tpl <- loadBody ident
  applyMustacheTemplate tpl ctx item

modificationTimeFieldWith :: TimeLocale     -- ^ Time output locale
                          -> String         -- ^ Key
                          -> String         -- ^ Format
                          -> MusContext a   -- ^ Resulting context
modificationTimeFieldWith locale key fmt = field key $ \i -> do
    mtime <- getItemModificationTime $ itemIdentifier i
    return $ formatTime locale fmt mtime

modificationTimeField :: String -> String -> MusContext a
modificationTimeField = modificationTimeFieldWith defaultTimeLocale

teaserFieldWithSeparator :: String              -- ^ Separator to use
                         -> String              -- ^ Key to use
                         -> Snapshot            -- ^ Snapshot to load
                         -> MusContext String   -- ^ Resulting context
teaserFieldWithSeparator separator key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    case needlePrefix separator body of
        Nothing -> throwError $ lines $
            "Hakyll.Web.Template.Context: no teaser defined for " ++
            show (itemIdentifier item)
        Just t -> return t

teaserField :: String           -- ^ Key to use
            -> Snapshot         -- ^ Snapshot to load
            -> MusContext String   -- ^ Resulting context
teaserField = teaserFieldWithSeparator teaserSeparator

teaserSeparator :: String
teaserSeparator = "<!--more-->"

runCtx :: MusContext a -> Item a -> Compiler Metadata
runCtx (SimpleContext md) _ = return md
runCtx (MusContext ctx) i   = ctx i

itemsFieldWithContext' :: MusContext a -> String -> (Item b -> Compiler [Item a]) -> MusContext b
itemsFieldWithContext' eachCtx key is = MusContext $ \i -> do
  mds <- mapM (fmap Object . runCtx eachCtx) =<< is i
  return $ HM.singleton (T.pack key) $ toJSON mds

itemsFieldWithContext :: MusContext a -> String -> [Item a] -> MusContext b
itemsFieldWithContext ctx key is = itemsFieldWithContext' ctx key (const $ return is)

itemsField :: ToJSON a => String -> [Item a] -> MusContext b
itemsField = itemsFieldWithContext defaultMusContext

boolField :: String -> (Item a -> Bool) -> MusContext a
boolField key f = field key (return . f)
