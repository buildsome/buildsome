{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Lib.Slave
  ( Slave, new
  , str
  , wait, waitCatch
  , cancel
  , wrap
  , When(..), Stats(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async)
import Data.Map (Map)
import Data.Monoid
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Lib.BuildMaps (TargetRep)
import Lib.FilePath (FilePath)
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified Lib.Printer as Printer

-- TODO: Get this Stats business out of here and have "Slave a"?
data When = FromCache | BuiltNow deriving Show

newtype Stats = Stats
  { statsSelfTime :: Map TargetRep (When, DiffTime)
  } deriving (Show, Monoid)

data Slave = Slave
  { slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  , slaveExecution :: Async Stats
  }

new :: Printer.Id -> [FilePath] -> IO Stats -> IO Slave
new printerId outputPaths action =
  Slave printerId outputPaths <$> Async.async action

str :: (Monoid str, IsString str) => Slave -> str
str slave =
  Printer.idStr (slavePrinterId slave) <> ": " <>
  fromString (show (slaveOutputPaths slave))

wait :: Slave -> IO Stats
wait = Async.wait . slaveExecution

waitCatch :: Slave -> IO (Either E.SomeException Stats)
waitCatch = Async.waitCatch . slaveExecution

cancel :: Slave -> IO ()
cancel = Async.cancel . slaveExecution

wrap :: (IO Stats -> IO Stats) -> Slave -> IO Slave
wrap f slave = do
  wrappedExecution <- (Async.async . f . Async.wait . slaveExecution) slave
  return slave { slaveExecution = wrappedExecution }
