{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase            #-}
{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards        #-}
module MissingSake
       ( setItemBody, tryWithFile, PageInfo(..), ContentsIndex(..)
       , Routing(..), itemPath
       , Patterns, (.&&.), (.||.), complement
       , (?===), conjoin, disjoin, globDirectoryFiles
       , replaceDir, withRouteRules, loadAllItemsAfter, loadOriginal, getSourcePath
       , loadContentsIndex, Snapshot, saveSnapshot, loadSnapshot, loadAllSnapshots
       ) where
import           Control.Monad       (forM, (<=<))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.Semigroup      (Semigroup, (<>))
import           Data.Store          (Store)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Web.Sake            (Action, FilePattern, Item (..),
                                      MonadAction, MonadSake, Readable, Rules,
                                      alternatives, copyFile', doesFileExist,
                                      getDirectoryFiles, itemBody,
                                      itemIdentifier, liftAction, loadItem,
                                      makeRelative, need, priority, putNormal,
                                      readFromBinaryFile', removeFilesAfter,
                                      runBinary, runIdentifier, writeBinaryFile,
                                      (</>), (?==), (?>), (~>))
import           Web.Sake.Conf       (SakeConf (..))

itemPath :: Item a -> FilePath
itemPath = runIdentifier . itemIdentifier

setItemBody :: a1 -> Item a2 -> Item a1
setItemBody bdy i = i { itemBody = bdy }

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

(?===) :: Patterns -> FilePath -> Bool
DNF cls ?=== fp = any (`clMatch` fp) cls

data Routing = ModifyPath (FilePath -> FilePath)
             | Copy
             | Create
             deriving (Generic)


applyRouting :: Routing -> FilePath -> FilePath
applyRouting Copy           fp = fp
applyRouting Create         fp = fp
applyRouting (ModifyPath f) fp = f fp

globDirectoryFiles :: FilePath -> Patterns -> Action [FilePath]
globDirectoryFiles dir (DNF cs) = fmap concat $ forM cs $ \(Clause ps ns) ->
  filter (\fp -> all (not . (?== fp)) ns) <$> getDirectoryFiles dir (HS.toList ps)

newtype PageInfo = PageInfo { sourcePath :: FilePath }
                 deriving (Read, Show, Eq, Ord, Generic)
                 deriving anyclass (Store)

newtype ContentsIndex =
  ContentsIndex { runContentsInfo :: HashMap FilePath PageInfo }
  deriving (Read, Show, Eq, Generic)
  deriving anyclass (Store)

pageListPath :: FilePath
pageListPath = "pages.bin"

-- | Creating routing and cleaning rules.
withRouteRules :: SakeConf -> [(Patterns, Routing)] -> Rules () -> Rules ()
withRouteRules SakeConf{..} rconfs rules = alternatives $ do
  "site" ~> do
    dic0 <- fmap (concat . reverse) $ forM rconfs $ \ (pats, r) -> do
      chs <- filter (not . ignoreFile) <$> globDirectoryFiles sourceDir pats
      forM chs $ \fp -> do
        let path = destinationDir </> applyRouting r fp
        return (path, PageInfo $ sourceDir </> fp)
    let cInd = ContentsIndex $ HM.fromList dic0
    writeBinaryFile (cacheDir </> pageListPath) cInd
    need $ map fst dic0

  "clean" ~> do
    removeFilesAfter destinationDir ["//*"]
    removeFilesAfter cacheDir ["//*"]

  rules

  let copyPats = disjoin $ map fst $
                 filter ((\case {Copy -> True; _ -> False}) . snd) rconfs

  (\fp -> not (ignoreFile fp) &&
          (copyPats ?=== makeRelative destinationDir fp)) ?> \out -> do
    putNormal $ "Falling back to copy rule " ++ out
    copyFile' (replaceDir destinationDir sourceDir out) out

loadAllItemsAfter :: FilePath -> Patterns -> Action [Item Text]
loadAllItemsAfter fp pats =
  mapM loadItem  =<< globDirectoryFiles fp pats

type Snapshot = String

saveSnapshot :: (Store a) => SakeConf -> Snapshot -> Item a -> Action (Item a)
saveSnapshot SakeConf{..} name i@Item{..} = do
  writeBinaryFile (replaceDir sourceDir (cacheDir </> name) (itemPath i)) itemBody
  return i

loadSnapshot :: (MonadSake m, Store a) => SakeConf -> Snapshot -> FilePath -> m (Item a)
loadSnapshot SakeConf{..} name fp =
  fmap runBinary <$> loadItem (cacheDir </> name </> fp)

loadAllSnapshots :: (Store a) => SakeConf -> Patterns -> Snapshot -> Action [Item a]
loadAllSnapshots SakeConf{..} pts name = do
  let snapD = cacheDir </> name
  mapM (fmap (fmap runBinary) . loadItem . (snapD </>)) =<< globDirectoryFiles snapD pts

hasSnapshot :: SakeConf -> Snapshot -> Item a -> Action Bool
hasSnapshot SakeConf{..} snap i =
  doesFileExist $ replaceDir sourceDir (cacheDir </> snap) (itemPath i)

replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir from to pth = to </> makeRelative from pth

loadOriginal :: Readable a => SakeConf -> FilePath -> Action (Item a)
loadOriginal cnf = loadItem <=< getSourcePath cnf

getSourcePath :: SakeConf -> FilePath -> Action FilePath
getSourcePath SakeConf{..} fp = do
  ContentsIndex dic <- readFromBinaryFile' (cacheDir </> pageListPath)
  case HM.lookup fp dic of
    Just PageInfo{..} -> return sourcePath
    Nothing           -> return $ replaceDir destinationDir sourceDir fp

loadContentsIndex :: SakeConf -> Action ContentsIndex
loadContentsIndex SakeConf{..} = readFromBinaryFile' (cacheDir </> pageListPath)
