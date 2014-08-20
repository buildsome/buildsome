{-# LANGUAGE OverloadedStrings #-}
module Buildsome.Slave
  ( Slave, new
  , target
  , str
  , wait, waitCatch
  , cancel
  ) where

import Buildsome.Stats (Stats(..))
import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async)
import Data.Monoid
import Data.String (IsString(..))
import Lib.FilePath (FilePath)
import Lib.Makefile (Target)
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified Lib.Printer as Printer

data Slave = Slave
  { slaveTarget :: Target
  , slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  , slaveExecution :: Async Stats
  }

target :: Slave -> Target
target = slaveTarget

new :: Target -> Printer.Id -> [FilePath] -> IO Stats -> IO Slave
new tgt printerId outputPaths action =
  Slave tgt printerId outputPaths <$> Async.async action

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
