{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..), InputSummary(..), ExecutionFilesSummary(..)
  , OutputDesc(..)
  , ExecutionLog(..), executionLog
  , ExecutionSummary(..), executionSummary
  , summarizeExecutionLog, appendSummary
  , FileContentDescCache(..), fileContentDescCache
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Prelude.Compat hiding (FilePath)

import           Buildsome.BuildId (BuildId)
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import           Data.Default (def)
import           Data.IORef
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import           Data.Time.Clock (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics (Generic)
import           Lib.Binary (encode, decode)
import           Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import           Lib.Exception (bracket)
import           Lib.FileDesc (FileContentDesc, FileModeDesc, FileStatDesc(..), fileModeDescOfStatDesc)
import           Lib.FilePath (FilePath, (<.>), (</>))
import           Lib.Makefile (Makefile)
import           Lib.Makefile.Monad (PutStrLn)
import           Lib.NonEmptyList (NonEmptyList(..))
import           Lib.StdOutputs (StdOutputs(..))
import           Lib.TimeInstances ()
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import qualified Database.LevelDB.Base as LevelDB
import qualified Lib.FSHook as FSHook
import qualified Lib.Makefile as Makefile
import qualified Lib.NonEmptyList as NonEmptyList
import qualified System.Posix.ByteString as Posix

schemaVersion :: ByteString
schemaVersion = "schema.ver.22"

data Db = Db
  { dbLevel             :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs     :: IORef (Set FilePath)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc  :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

data Reason
  = BecauseSpeculative Reason
  | BecauseHintFrom [FilePath]
  | BecauseHooked FSHook.AccessDoc
  | BecauseContainerDirectoryOfInput Reason FilePath
  | BecauseContainerDirectoryOfOutput FilePath
  | BecauseInput Reason FilePath
  | BecauseRequested ByteString
  deriving (Generic, Show)
instance Binary Reason


data InputDesc a = InputDesc
  { idModeAccess    :: Maybe (a, FileModeDesc)
  , idStatAccess    :: Maybe (a, FileStatDesc)
  , idContentAccess :: Maybe (a, FileContentDesc)
  } deriving (Generic, Show, Functor)

instance Binary a => Binary (InputDesc a)

data FileDesc ne e
  = FileDescNonExisting !ne
  | FileDescExisting !e
  deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

bimapFileDesc :: (ne -> ne') -> (e -> e') -> FileDesc ne e -> FileDesc ne' e'
bimapFileDesc f _ (FileDescNonExisting x) = FileDescNonExisting (f x)
bimapFileDesc _ g (FileDescExisting x) = FileDescExisting (g x)

data OutputDesc = OutputDesc
  { odStatDesc    :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show)
instance Binary OutputDesc

data InputSummary
  = InputMTime POSIXTime (InputDesc ())
  -- For dirs, this is for stat/existence only check
  | InputModeDesc FileModeDesc
  deriving (Generic, Show)
instance Binary InputSummary

data ExecutionFilesSummary = ExecutionFilesSummary
  { efsInputsDescs  :: Map FilePath (FileDesc () InputSummary)
  , efsOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , efsSelfTime     :: DiffTime
  , efsStdErr       :: ByteString
  } deriving (Generic, Show)
instance Binary ExecutionFilesSummary

data ExecutionSummary = ExecutionSummary
  { esFilesSummary :: NonEmptyList (POSIXTime, ExecutionFilesSummary)
  } deriving (Generic, Show)
instance Binary ExecutionSummary

data ExecutionLog = ExecutionLog
  { elBuildId      :: BuildId
  , elCommand      :: ByteString -- Mainly for debugging
  , elInputsDescs  :: Map FilePath (FileDesc Reason (POSIXTime, InputDesc Reason))
  , elOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , elStdoutputs   :: StdOutputs ByteString
  , elSelfTime     :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

appendSummary :: POSIXTime -> ExecutionFilesSummary -> ExecutionSummary -> ExecutionSummary
appendSummary time efs ExecutionSummary{..} = ExecutionSummary { esFilesSummary = NonEmptyList.cons (time, efs) esFilesSummary }

summarizeExecutionLog :: ExecutionLog -> ExecutionFilesSummary
summarizeExecutionLog ExecutionLog{..} =
  ExecutionFilesSummary
  { efsInputsDescs = bimapFileDesc (const ()) summarizeInput <$> elInputsDescs
  , efsOutputsDescs = elOutputsDescs
  , efsSelfTime = elSelfTime
  , efsStdErr = stdErr elStdoutputs
  }
  where
    dropInputDescReason = fmap (const ())
    summarizeInput (mtime, inputDesc) =
      case inputDesc of
        InputDesc
          { idModeAccess = Just (_, modeDesc)
          , idStatAccess = Nothing
          , idContentAccess = Nothing } -> InputModeDesc  modeDesc
        InputDesc { idContentAccess = Just _ } -> InputMTime mtime $ dropInputDescReason inputDesc
        InputDesc { idStatAccess = Just (_, s@FileStatDirectory{}) } ->
            InputModeDesc $ fileModeDescOfStatDesc s
        InputDesc { idStatAccess = Just _ } -> InputMTime mtime $ dropInputDescReason inputDesc
        InputDesc Nothing Nothing Nothing -> error "Input accessed but no access type?!"

registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = LevelDB.put (dbLevel db) def key $ encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = fmap decode <$> LevelDB.get (dbLevel db) def key

deleteKey :: Db -> ByteString -> IO ()
deleteKey db = LevelDB.delete (dbLevel db) def

options :: LevelDB.Options
options =
    LevelDB.defaultOptions
    { LevelDB.createIfMissing = True
    , LevelDB.errorIfExists = False
    }

withLevelDb :: FilePath -> (LevelDB.DB -> IO a) -> IO a
withLevelDb dbPath =
  LevelDB.withDB (BS8.unpack (dbPath </> schemaVersion)) options

with :: FilePath -> (Db -> IO a) -> IO a
with rawDbPath body = do
  dbPath <- makeAbsolutePath rawDbPath
  createDirectories dbPath
  withLevelDb dbPath $ \levelDb ->
    withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
    withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
    body (Db levelDb registeredOutputs leakedOutputs)
  where
    withIORefFile path =
      bracket (newIORef =<< decodeFileOrEmpty path) (writeBack path)
    writeBack path ref = do
      BS8.writeFile (BS8.unpack (path <.> "tmp")) .
        BS8.unlines . S.toList =<< readIORef ref
      Posix.rename (path <.> "tmp") path
    decodeFileOrEmpty path =
      (S.fromList . BS8.lines <$> BS8.readFile (BS8.unpack path))
      `catchDoesNotExist` return S.empty

data IRef a = IRef
  { readIRef  :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef   :: IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

data ExecutionLogType = ExecutionLogTypeSummary  | ExecutionLogTypeFull
    deriving (Generic)

instance Binary ExecutionLogType

targetKey :: Makefile.TargetType output input -> ByteString
targetKey = MD5.hash . Makefile.targetCmds

executionLog :: Makefile.Target -> Db -> IRef ExecutionLog
executionLog = mkIRefKey . (encode ExecutionLogTypeFull <>) . targetKey

executionSummary :: Makefile.Target -> Db -> IRef ExecutionSummary
executionSummary = mkIRefKey . (encode ExecutionLogTypeSummary <>) .  targetKey

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache = mkIRefKey

type MFileContentDesc = FileDesc () FileContentDesc

data MakefileParseCache = MakefileParseCache
  { mpcInputs :: (FilePath, Map FilePath MFileContentDesc)
  , mpcOutput :: (Makefile, [PutStrLn])
  } deriving (Generic)
instance Binary MakefileParseCache

makefileParseCache :: Db -> Makefile.Vars -> IRef MakefileParseCache
makefileParseCache db vars =
    mkIRefKey ("makefileParseCache_Schema.1:" <> MD5.hash (encode vars)) db
