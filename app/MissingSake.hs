{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies          #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase  #-}
{-# LANGUAGE NamedFieldPuns, NoMonomorphismRestriction, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving                                         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module MissingSake
       ( tryWithFile, PageInfo(..), ContentsIndex(..)
       , Routing(..), (</?>), hasSnapshot
       , Patterns, (.&&.), (.||.), complement, stripDirectory
       , (?===), (%%>), conjoin, disjoin, globDirectoryFiles, ifChanged
       , replaceDir, withRouteRules, loadAllItemsAfter, loadOriginal, getSourcePath
       , loadContentsIndex, Snapshot, saveSnapshot, loadSnapshot, loadAllSnapshots
       ) where
import           Control.Monad              (forM, when, (<=<))
import           Crypto.Hash.SHA256         (hash)
import           Data.Aeson                 (Value)
import qualified Data.Binary                as Bin
import qualified Data.ByteString            as BS
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.List                  as L
import           Data.Scientific            (Scientific (..))
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Store                 (Store (..))
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           System.Directory           (createDirectoryIfMissing)
import           System.IO                  (IOMode (..), withFile)
import           Web.Sake                   (Action, FilePattern,
                                             Identifier (..), Item (..),
                                             Metadata, MonadAction, MonadSake,
                                             Readable, Rules, alternatives,
                                             copyFile', doesFileExist,
                                             filePattern, getDirectoryFiles,
                                             itemBody, itemIdentifier, itemPath,
                                             liftAction, liftIO, loadItem,
                                             makeRelative, need, putNormal,
                                             readFromBinaryFile',
                                             removeFilesAfter, runIdentifier,
                                             withTempFile, writeBinaryFile,
                                             (%>), (<//>), (</>), (?==), (?>),
                                             (~>))
import           Web.Sake.Conf              (SakeConf (..))

tryWithFile :: MonadAction m => FilePath -> m a -> m (Maybe a)
tryWithFile fp act = do
  ex <- liftAction $ doesFileExist fp
  if ex
    then Just <$> act
    else return Nothing

data Clause = Clause { _positives :: HS.HashSet FilePattern
                     , _negatives :: HS.HashSet FilePattern
                     }
            deriving (Read, Show, Eq, Generic, Store)

newtype Patterns = DNF [Clause]
                   -- ^ Disjunction Normal Form (disjunction of conjunctions of literals)
                 deriving (Read, Show, Eq, Generic)
                 deriving newtype (Store)

instance IsString Patterns where
  fromString = DNF . pure . flip Clause HS.empty . HS.singleton . fromString


instance Semigroup Clause

instance Monoid Clause where
  mempty = Clause HS.empty HS.empty
  mappend (Clause ls rs) (Clause us ts) = Clause (ls <> us) (rs <> ts)

infixr 2 .||.
infixr 3 .&&.

(.||.) :: Patterns -> Patterns -> Patterns
DNF xs .||. DNF ys = DNF (xs ++ ys)

(.&&.) :: Patterns -> Patterns -> Patterns
DNF xs .&&. DNF ys =
  removeRedundants $ DNF $ concatMap (\x -> map (x <>) ys) xs

negateClause :: Clause -> Patterns
negateClause (Clause ls rs) = DNF $ [ Clause HS.empty (HS.singleton f) | f <- HS.toList ls]
                                 ++ [ Clause (HS.singleton g) HS.empty | g <- HS.toList rs]

complement :: Patterns -> Patterns
complement (DNF fs) = foldr1 (.&&.) $ map negateClause fs

removeRedundants :: Patterns -> Patterns
removeRedundants (DNF cs) = DNF $ filter (\(Clause ps ns) -> HS.null $ ps `HS.intersection` ns) cs

clMatch :: Clause -> FilePath -> Bool
clMatch (Clause ps ns) fp =
  all (?== fp) ps && all (not . (?== fp)) ns

conjoin :: Foldable t => t Patterns -> Patterns
conjoin = foldr1 (.&&.)

disjoin :: Foldable t => t Patterns -> Patterns
disjoin = foldr1 (.||.)

infix 4 ?===
(?===) :: Patterns -> FilePath -> Bool
DNF cls ?=== fp = any (`clMatch` fp) cls

infix 1 %%>

(%%>) :: Patterns -> (FilePath -> Action ()) -> Rules ()
pats %%> act = (pats ?===) ?> act

data Routing = Convert Patterns (FilePath -> FilePath)
             | Copy Patterns
             | Create FilePath
             deriving (Generic)

generatePageInfo :: SakeConf -> Patterns -> (FilePath -> FilePath) -> Action [(FilePath, PageInfo)]
generatePageInfo SakeConf{..} pats f = do
  chs <- filter (not . ignoreFile) <$> globDirectoryFiles sourceDir pats
  forM chs $ \fp -> do
    let path = destinationDir </> f fp
    return (path, PageInfo $ Just $ sourceDir </> fp)

globDirectoryFiles :: FilePath -> Patterns -> Action [FilePath]
globDirectoryFiles dir (DNF cs) = fmap concat $ forM cs $ \(Clause ps ns) ->
  filter (\fp -> all (not . (?== fp)) ns) <$> getDirectoryFiles dir (HS.toList ps)

newtype PageInfo = PageInfo { sourcePath :: Maybe FilePath }
                 deriving (Read, Show, Eq, Ord, Generic)
                 deriving anyclass (Store)

newtype ContentsIndex =
  ContentsIndex { runContentsInfo :: HashMap FilePath PageInfo }
  deriving (Read, Show, Eq, Generic)
  deriving anyclass (Store)

pageListPath :: FilePath
pageListPath = "pages.bin"

stripDirectory :: FilePath -> FilePath -> Maybe FilePath
stripDirectory parent target
  | parent `L.isPrefixOf` target = Just $ makeRelative parent target
  | otherwise = Nothing

-- | Creating routing and cleaning rules.
withRouteRules :: SakeConf -> [Routing] -> Rules () -> Rules ()
withRouteRules sakeConf@SakeConf{..} rconfs rules = alternatives $ do
  "site" ~> do
    liftIO $ do
      createDirectoryIfMissing True destinationDir
      createDirectoryIfMissing True sourceDir
      createDirectoryIfMissing True snapshotDir
    ContentsIndex dic0 <- readFromBinaryFile' (cacheDir </> pageListPath)
    putNormal $ "needing: " ++ show (map fst $ HM.toList dic0)
    need $ map fst $ HM.toList dic0

  cacheDir </> pageListPath %> \out -> do
    dic0 <- fmap (concat . reverse) $ forM rconfs $ \case
      Convert pats f -> generatePageInfo sakeConf pats f
      Copy pats -> generatePageInfo sakeConf pats id
      Create fp -> return [(destinationDir </> fp, PageInfo Nothing)]
    writeBinaryFile out $ ContentsIndex $ HM.fromList dic0

  snapshotDir </> "*" <//> "*" %> \out -> do
    let Just [_, rest, fname] = filePattern (snapshotDir </> "*" <//> "*") out
    need [sourceDir </> rest </> fname]

  "clean" ~> do
    removeFilesAfter destinationDir ["//*"]
    removeFilesAfter cacheDir ["//*"]
    removeFilesAfter snapshotDir ["//*"]

  rules

  let copyPats = disjoin $
                 foldMap (\case {Copy pat -> [pat]; Create fp -> [fromString fp]; _ -> []}) rconfs

  (\fp -> not (ignoreFile fp) &&
          maybe False (copyPats ?===) (stripDirectory destinationDir fp)) ?> \out -> do
    let orig = replaceDir destinationDir sourceDir out
    putNormal $ "Falling back to copy rule: " ++ out ++ "; copied from: " ++ orig
    copyFile' orig out

loadAllItemsAfter :: FilePath -> Patterns -> Action [Item Text]
loadAllItemsAfter fp pats =
  mapM (loadItem . (fp </>)) =<< globDirectoryFiles fp pats

type Snapshot = String

instance Store Scientific where
  size = contramap Bin.encode size
  peek = Bin.decode <$> peek
  poke = poke . Bin.encode

deriving instance Store Value

data Snapshotted a = Snapshotted { snapBody       :: a
                                 , snapIdentifier :: FilePath
                                 , snapMetadata   :: Metadata
                                 }
                   deriving (Read, Show, Eq, Generic, Store)

saveSnapshot :: (Store a) => SakeConf -> Snapshot -> Item a -> Action (Item a)
saveSnapshot SakeConf{..} name i@Item{..} = do
  writeBinaryFile (replaceDir sourceDir (snapshotDir </> name) (itemPath i))
    Snapshotted { snapBody = itemBody
                , snapIdentifier = runIdentifier itemIdentifier
                , snapMetadata = itemMetadata
                }
  return i

snapToItem :: Snapshotted a -> Item a
snapToItem Snapshotted{..} =
  Item { itemIdentifier = Identifier snapIdentifier
       , itemBody       = snapBody
       , itemMetadata   = snapMetadata
       }

loadSnapshot :: (MonadSake m, Store a) => SakeConf -> Snapshot -> FilePath -> m (Item a)
loadSnapshot SakeConf{..} name fp =
  snapToItem <$> readFromBinaryFile' (snapshotDir </> name </> fp)

loadAllSnapshots :: (Store a) => SakeConf -> Patterns -> Snapshot -> Action [Item a]
loadAllSnapshots SakeConf{..} pts name = do
  let snapD = snapshotDir </> name
  mapM (fmap snapToItem . readFromBinaryFile' . (snapD </>)) =<< globDirectoryFiles snapD pts

hasSnapshot :: SakeConf -> Snapshot -> Item a -> Action Bool
hasSnapshot SakeConf{..} snap i =
  doesFileExist $ replaceDir sourceDir (snapshotDir </> snap) (itemPath i)

replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir from to pth = to </> makeRelative from pth

loadOriginal :: Readable a => SakeConf -> FilePath -> Action (Item a)
loadOriginal cnf = loadItem <=< getSourcePath cnf

getSourcePath :: SakeConf -> FilePath -> Action FilePath
getSourcePath SakeConf{..} fp = do
  ContentsIndex dic <- readFromBinaryFile' (cacheDir </> pageListPath)
  case HM.lookup fp dic of
    Just PageInfo{ sourcePath = Just pth } -> return pth
    _           -> error $ "No Source Path found: " ++ fp

ifChanged :: (FilePath -> a -> Action ()) -> FilePath -> a -> Action ()
ifChanged write fp bdy = do
  exist <- doesFileExist fp
  if not exist
    then write fp bdy
    else withTempFile $ \tmp -> do
    write tmp bdy
    b <- liftIO $ withFile tmp ReadMode $ \htmp -> withFile fp ReadMode $ \h -> do
      stmp <- BS.hGetContents htmp
      src <- BS.hGetContents h
      return (hash src /= hash stmp)
    when b $ write fp bdy

loadContentsIndex :: SakeConf -> Action ContentsIndex
loadContentsIndex SakeConf{..} = readFromBinaryFile' (cacheDir </> pageListPath)

infixr 5 </?>
(</?>) :: FilePath -> Patterns -> Patterns
dir </?> DNF cls = fromString (dir ++ "//*") .&&. DNF (map go cls)
  where
    go (Clause ps ns) = Clause (HS.map (dir </>) ps) (HS.map (dir </>) ns)
