{-# LANGUAGE DeriveGeneric #-}
module Db
  ( Db, with
  , InputAccess(..)
  , ExecutionLog(..), writeExecutionLog, readExecutionLog
  , readRegisteredOutputs, writeRegisteredOutputs
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary, get, put)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lib.Binary (runGet, runPut)
import Lib.FileDesc (FileDesc, FileModeDesc)
import Lib.Makefile (TargetType(..), Target)
import Lib.StdOutputs (StdOutputs(..))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Database.Sophia as Sophia

newtype Db = Db Sophia.Db

data InputAccess = InputAccessModeOnly FileModeDesc | InputAccessFull FileDesc
  deriving (Generic, Show)
instance Binary InputAccess

data ExecutionLog = ExecutionLog
  { _elInputsDescs :: Map FilePath InputAccess
  , _elOutputsDescs :: Map FilePath FileDesc
  , _elStdoutputs :: [StdOutputs] -- Of each command
  } deriving (Generic, Show)
instance Binary ExecutionLog

targetKey :: Target -> ByteString
targetKey target =
  MD5.hash $ BS.pack (unlines (targetCmds target)) -- TODO: Canonicalize commands (whitespace/etc)

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey (Db db) key val = Sophia.setValue db key $ runPut $ put val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey (Db db) key = fmap (runGet get) <$> Sophia.getValue db key

with :: FilePath -> (Db -> IO a) -> IO a
with dbFileName body = do
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation dbFileName
    Sophia.withDb env $ \db ->
      body (Db db)

registeredOutputsKey :: ByteString
registeredOutputsKey = BS.pack "outputs"

readRegisteredOutputs :: Db -> IO (Set FilePath)
readRegisteredOutputs db =
  fromMaybe S.empty <$> getKey db registeredOutputsKey

writeRegisteredOutputs :: Db -> Set FilePath -> IO ()
writeRegisteredOutputs db outputs =
  setKey db registeredOutputsKey outputs

writeExecutionLog :: Db -> Target -> ExecutionLog -> IO ()
writeExecutionLog db target execLog =
  setKey db (targetKey target) execLog

readExecutionLog :: Db -> Target -> IO (Maybe ExecutionLog)
readExecutionLog db = getKey db . targetKey
