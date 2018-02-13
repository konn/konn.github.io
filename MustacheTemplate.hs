{-# LANGUAGE ExistentialQuantification, ExtendedDefaultRules               #-}
{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses    #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module MustacheTemplate
       ( MusContext (), applyMustache, loadAndApplyMustache
       , applyAsMustache, applyMustacheWithHakyllContext
       , metadataField, field, constField, bodyField, urlField
       , pathField, titleField, dateFieldWith, dateField
       , defaultMusContext, modificationTimeFieldWith, modificationTimeField
       , teaserField, teaserFieldWithSeparator, boolField
       , itemsFieldWithContext', itemsFieldWithContext, itemsField
       , musContextToContext
       )
       where
import Instances ()

import           Control.Applicative         (empty)
import           Control.Monad.Except        (catchError, throwError)
import           Control.Monad.RWS           hiding ((<>))
import           Data.Aeson.Types
import qualified Data.Foldable               as F
import qualified Data.HashMap.Lazy           as HM
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.Map                    as M
import           Data.Semigroup
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as LT
import           Data.Time
import qualified Data.Yaml                   as Y
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Metadata
import           Hakyll.Core.Util.String     (needlePrefix)
import           Hakyll.Web.Html
import           Hakyll.Web.Template.Context (getItemModificationTime,
                                              getItemUTC)
import qualified Hakyll.Web.Template.Context as H
import           System.FilePath             (takeBaseName)
import           Text.Megaparsec             (parseErrorPretty', unPos)
import           Text.Mustache

default ([])

data MusContext a = SimpleContext Metadata
                  | MusContext (Item a -> Compiler Metadata)

instance Semigroup (MusContext a) where
  SimpleContext val <> SimpleContext val' = SimpleContext $ val <> val'
  MusContext acq <> SimpleContext val' = MusContext $ \i -> do
    val <- acq i
    return $ val <> val'
  SimpleContext val <> MusContext get' = MusContext $ \i -> do
    val' <- get' i
    return $ val <> val'
  MusContext acq <> MusContext get' = MusContext $ \i -> do
    (<>) <$> acq i <*> get' i


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
      src   = T.pack $ itemBody item
  in case compileMustacheText fname $ src of
    Left err  -> throwError [ parseErrorPretty' src err ]
    Right tpl -> applyMustache tpl ctx item

applyMustache :: Template -> MusContext a -> Item a -> Compiler (Item String)
applyMustache tmpl (SimpleContext val) i = do
  debugCompiler $
    concat [ "\tApplying template "
           , show tmpl
           , " to a simple context "
           , show val]
  return $ itemSetBody (LT.unpack $ renderMustache tmpl $ toJSON val) i
applyMustache tmpl (MusContext f) i = do
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
  applyMustache tpl ctx item

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

musContextToContext :: forall a. (ToJSON a, FromJSON a)
                    => MusContext a -> H.Context a
musContextToContext cxt =
  case cxt of
    SimpleContext dic -> go $ const $ return dic
    MusContext acq    -> go acq
  where
    go acq = H.Context $ \k _ i -> do
      dic <- acq i
      case HM.lookup (T.pack k) dic of
        Just (String str) -> return $ H.StringField $ T.unpack str
        Just (Array v0) | Success vals <- mapM fromJSON v0 -> do
          items <- mapM makeItem $ F.toList vals
          return $ H.ListField (musContextToContext cxt) items
        Just (Bool True) -> return $ H.StringField ""
        Just (Bool False) -> empty
        Just Null -> empty
        Just n@Number{} -> return $ H.StringField $ T.unpack $  T.decodeUtf8 $ Y.encode n
        Just val -> throwError
                    [ "Cannot convert the value of " <> show k <> " to Hakyll Context: "
                    , T.unpack $ T.decodeUtf8 $ Y.encode val
                    ]
        Nothing -> throwError ["There is no key " <> show k]

type CtxMachine = RWST (M.Map PName [Node]) () (M.Map PName T.Text) Compiler

applyMustacheWithHakyllContext :: ToJSON a
                               => Template -> H.Context a -> Item a -> Compiler (Item String)
applyMustacheWithHakyllContext (Template focus dic) cxt i = do
  bdy <- T.unpack . fst <$> evalRWST (applyFor i focus cxt) dic mempty
  return $ itemSetBody bdy i

applyFor :: Item a -> PName -> H.Context a -> CtxMachine T.Text
applyFor i0 focus c0 = gets (M.lookup focus) >>= \case
  Just t -> return t
  Nothing -> asks (M.lookup focus) >>= \case
    Nothing -> throwError ["No template found: " ++ show focus]
    Just ns -> mconcat <$> mapM (goNode i0 c0) ns

goNode :: Item a -> H.Context a -> Node -> CtxMachine T.Text
goNode _ _ (TextBlock t)         = return t
goNode i (H.Context toCxt) (EscapedVar (Key [k])) = lift (toCxt (T.unpack k) [] i) >>= \case
  H.StringField str -> return $ T.pack $ escapeHtml $ str
  _ -> throwError ["Key " ++ show k ++ " accepts only single value, not a list"]
goNode i (H.Context toCxt) (UnescapedVar (Key [k])) = lift (toCxt (T.unpack k) [] i) >>= \case
  H.StringField str -> return $ T.pack $ str
  _ -> throwError ["Key " ++ show k ++ " accepts only single value, not a list"]
goNode _ _ EscapedVar {} = throwError ["Nested keys are not supported for Hakyll Template"]
goNode _ _ UnescapedVar {} = throwError ["Nested keys are not supported for Hakyll Template"]
goNode i c@(H.Context toCxt) (Section (Key [k]) ns) = lift (toCxt (T.unpack k) [] i) >>= \case
  H.StringField str ->
    if null str
    then return ""
    else mconcat <$> mapM (goNode i c) ns
  H.ListField c' is ->
    mconcat <$> mapM (\i' -> mconcat <$> mapM (goNode i' c') ns) is
goNode _ _ Section{} = throwError ["Nested keys are not supported for Hakyll Template"]
goNode i c@(H.Context toCxt) (InvertedSection (Key [k]) ns) =
  sub `catchError` \_ -> mconcat <$> mapM (goNode i c) ns
  where
    sub = lift (toCxt (T.unpack k) [] i) >>= \case
      H.StringField "" -> mconcat <$> mapM (goNode i c) ns
      H.ListField _ [] -> mconcat <$> mapM (goNode i c) ns
      _ -> return ""

goNode _ _ InvertedSection{} = throwError ["Nested keys are not supported for Hakyll Template"]
goNode i c (Partial p (Just lvl)) =
  T.unlines . map (<> T.replicate (unPos lvl) " ") . T.lines <$> applyFor i p c
goNode i c (Partial p Nothing) = applyFor i p c
