{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..), bimapFileDesc
  , OutputDesc(..)
  , ExecutionLog(..), executionLogTree
  , InputLog(..), InputLogStat(..), ExecutionLogTree(..), ExecutionLogTreeInput(..)
  , FileContentDescCache(..), fileContentDescCache
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Buildsome.BuildId (BuildId)
import qualified Crypto.Hash.MD5 as MD5
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Default (def)
import           Data.IORef
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Time.Clock (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Database.LevelDB.Base as LevelDB
import           GHC.Generics (Generic)
import           Lib.Binary (encode, decode)
import           Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import           Lib.Exception (bracket)
import qualified Lib.FSHook as FSHook
import           Lib.FileDesc (FileContentDesc, FileModeDesc, FileStatDesc, BasicStatEssence)
import           Lib.FilePath (FilePath, (</>), (<.>))
import           Lib.Posix.FileType (FileType)
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Makefile.Monad (PutStrLn)
import           Lib.NonEmptyMap (NonEmptyMap)

import           Lib.StdOutputs (StdOutputs(..))
import           Lib.TimeInstances ()
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath)

schemaVersion :: ByteString
schemaVersion = "schema.ver.21"

data Db = Db
  { dbLevel :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

data Reason
  = BecauseSpeculative Reason
  | BecauseHintFrom [FilePath]
  | BecauseHooked FSHook.AccessDoc
  | BecauseChildOfFullyRequestedDirectory Reason
  | BecauseContainerDirectoryOfInput Reason FilePath
  | BecauseContainerDirectoryOfOutput FilePath
  | BecauseInput Reason FilePath
  | BecauseRequested ByteString
  deriving (Generic, Show)
instance Binary Reason


data InputDesc = InputDesc
  { idModeAccess :: Maybe (Reason, FileModeDesc)
  , idStatAccess :: Maybe (Reason, FileStatDesc)
  , idContentAccess :: Maybe (Reason, FileContentDesc)
  } deriving (Generic, Show)
instance Binary InputDesc

data FileDesc ne e
  = FileDescNonExisting ne
  | FileDescExisting e
  deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

bimapFileDesc :: (ne -> ne') -> (e -> e') -> FileDesc ne e -> FileDesc ne' e'
bimapFileDesc f _ (FileDescNonExisting x) = FileDescNonExisting (f x)
bimapFileDesc _ g (FileDescExisting x) = FileDescExisting (g x)

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show, Eq)
instance Binary OutputDesc

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elCommand :: ByteString -- Mainly for debugging
  , elInputsDescs :: Map FilePath (FileDesc Reason (POSIXTime, InputDesc))
  , elOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , elStdoutputs :: StdOutputs ByteString
  , elSelfTime :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

data InputLogStat = InputLogStat
  { ilsBasicStatEssence      :: BasicStatEssence
  , ilsFileSize              :: Maybe Posix.FileOffset
  , ilsFileType              :: Maybe FileType
  } deriving (Generic, Show, Eq, Ord)
instance Binary InputLogStat

data InputLog = InputLog
  { ilModeAccess :: Maybe FileModeDesc
  , ilStatAccess :: Maybe InputLogStat
  , ilContentAccess :: Maybe FileContentDesc
  } deriving (Generic, Show, Eq, Ord)
instance Binary InputLog

data ExecutionLogTreeInput = ExecutionLogTreeInput
  { eltiBranches :: NonEmptyMap (FileDesc () InputLog) ExecutionLogTree
  } deriving (Generic, Show)
instance Binary ExecutionLogTreeInput

data ExecutionLogTree
  = ExecutionLogBranch (NonEmptyMap FilePath ExecutionLogTreeInput)
  | ExecutionLogLeaf ExecutionLog
  deriving (Generic, Show)
instance Binary ExecutionLogTree

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
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef :: IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

executionLogTree :: Makefile.Target -> Db -> IRef ExecutionLogTree
executionLogTree target = mkIRefKey targetKey
  where
    targetKey = MD5.hash $ Makefile.targetCmds target -- TODO: Canonicalize commands (whitespace/etc)

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
