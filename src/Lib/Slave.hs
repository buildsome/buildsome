{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Lib.Slave
  ( Slave, new
  , str
  , wait
  , cancel
  , wrap
  , Stats(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async)
import Data.Map (Map)
import Data.Monoid
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Lib.Async (verifyCancelled)
import Lib.BuildMaps (TargetRep)
import Lib.FilePath (FilePath)
import Lib.Printer (Printer)
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified Lib.Printer as Printer

newtype Stats = Stats
  { statsSelfTime :: Map TargetRep DiffTime
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

wait :: Printer -> Slave -> IO Stats
wait _printer slave =
  -- Keep this around so we can enable logging about slave waits
  -- easily:
  -- Printer.printWrap _printer ("Waiting for " <> str slave) $
  Async.wait $ slaveExecution slave

cancel :: Slave -> IO (Either E.SomeException Stats)
cancel = verifyCancelled . slaveExecution

wrap :: (IO Stats -> IO Stats) -> Slave -> IO Slave
wrap f slave = do
  wrappedExecution <- (Async.async . f . Async.wait . slaveExecution) slave
  return slave { slaveExecution = wrappedExecution }
