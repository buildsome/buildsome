{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Db
  ( Db, with
  , InputAccess(..)
  , ExecutionLog(..), Reason
  , IRef, readIRef, writeIRef
  , executionLog
  , registeredOutputs, readRegisteredOutputs
  , leakedOutputs, readLeakedOutputs
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lib.Binary (encode, decode)
import Lib.Directory (catchDoesNotExist)
import Lib.FileDesc (FileDesc, FileModeDesc)
import Lib.Makefile (TargetType(..), Target)
import Lib.StdOutputs (StdOutputs(..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, renameFile)
import System.FilePath ((</>), (<.>))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Database.Sophia as Sophia

schemaVersion :: String
schemaVersion = "schema.ver.1"

data Db = Db
  { dbSophia :: Sophia.Db
  , dbDirectory :: FilePath
  }

type Reason = String

data InputAccess = InputAccessModeOnly FileModeDesc | InputAccessFull FileDesc
  deriving (Generic, Show)
instance Binary InputAccess

data ExecutionLog = ExecutionLog
  { _elInputsDescs :: Map FilePath (Reason, InputAccess)
  , _elOutputsDescs :: Map FilePath FileDesc
  , _elStdoutputs :: StdOutputs
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
      body (Db db dbPath)

data IRef a = IRef
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  }

mkIRefFile :: Binary a => String -> Db -> IRef a
mkIRefFile fileName db = IRef
  { readIRef = (Just . decode <$> BS.readFile path) `catchDoesNotExist` return Nothing
  , writeIRef = \val -> do
      BS.writeFile tmpPath (encode val)
      renameFile tmpPath path
  }
  where
    tmpPath = path <.> "tmp"
    path = dbDirectory db </> fileName

registeredOutputs :: Db -> IRef (Set FilePath)
registeredOutputs = mkIRefFile "outputs"

readRegisteredOutputs :: Db -> IO (Set FilePath)
readRegisteredOutputs db = fromMaybe S.empty <$> readIRef (registeredOutputs db)

-- We allow leakage of "legal" outputs (e.g: .pyc files) but we don't
-- want them registered as outputs that may disappear from Makefile
-- and thus be deleted
leakedOutputs :: Db -> IRef (Set FilePath)
leakedOutputs = mkIRefFile "leaked_outputs"

readLeakedOutputs :: Db -> IO (Set FilePath)
readLeakedOutputs db = fromMaybe S.empty <$> readIRef (leakedOutputs db)

executionLog :: Target -> Db -> IRef ExecutionLog
executionLog target = mkIRefKey targetKey
  where
    targetKey =
      MD5.hash $ BS.pack (targetCmds target) -- TODO: Canonicalize commands (whitespace/etc)
