{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputAccess(..)
  , ExecutionLog(..), Reason
  , IRef, readIRef, writeIRef
  , executionLog
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lib.Binary (encode, decode)
import Lib.BuildId (BuildId)
import Lib.Directory (catchDoesNotExist)
import Lib.FileDesc (FileDesc, FileModeDesc)
import Lib.Makefile (TargetType(..), Target)
import Lib.StdOutputs (StdOutputs(..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, renameFile)
import System.FilePath ((</>), (<.>))
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Database.Sophia as Sophia

schemaVersion :: String
schemaVersion = "schema.ver.2"

data Db = Db
  { dbSophia :: Sophia.Db
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  }

registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

type Reason = String

data InputAccess = InputAccessModeOnly FileModeDesc | InputAccessFull FileDesc
  deriving (Generic, Show)
instance Binary InputAccess

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elInputsDescs :: Map FilePath (Reason, InputAccess)
  , elOutputsDescs :: Map FilePath FileDesc
  , elStdoutputs :: StdOutputs
  } deriving (Generic, Show)
instance Binary ExecutionLog

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = Sophia.setValue (dbSophia db) key $ encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = fmap decode <$> Sophia.getValue (dbSophia db) key

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath path = (</> path) <$> getCurrentDirectory

with :: FilePath -> (Db -> IO a) -> IO a
with rawDbPath body = do
  dbPath <- makeAbsolutePath rawDbPath
  createDirectoryIfMissing False dbPath
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation (dbPath </> schemaVersion)
    Sophia.withDb env $ \db ->
      withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
      withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
      body (Db db registeredOutputs leakedOutputs)
  where
    readOrError path str =
      case reads str of
      [(x, "")] -> x
      _ -> error $ "Failed to parse: " ++ show path
    withIORefFile path =
      E.bracket (newIORef =<< decodeFileOrEmpty path)
                (writeBack path)
    writeBack path ref = do
      writeFile (path <.> "tmp") . show . S.toList =<< readIORef ref
      renameFile (path <.> "tmp") path
    decodeFileOrEmpty path = (S.fromList . readOrError path <$> readFile path) `catchDoesNotExist` return S.empty

data IRef a = IRef
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  }

executionLog :: Target -> Db -> IRef ExecutionLog
executionLog target = mkIRefKey targetKey
  where
    targetKey =
      MD5.hash $ BS.pack (targetCmds target) -- TODO: Canonicalize commands (whitespace/etc)
