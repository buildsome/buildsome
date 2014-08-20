{-# LANGUAGE OverloadedStrings #-}
module Buildsome.Slave
  ( Slave, new
  , target
  , str
  , wait, waitCatch
  , cancel
  ) where

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

data Slave a = Slave
  { slaveTarget :: Target
  , slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  , slaveExecution :: Async a
  }

target :: Slave a -> Target
target = slaveTarget

new :: Target -> Printer.Id -> [FilePath] -> IO a -> IO (Slave a)
new tgt printerId outputPaths action =
  Slave tgt printerId outputPaths <$> Async.async action

str :: (Monoid str, IsString str) => Slave a -> str
str slave =
  Printer.idStr (slavePrinterId slave) <> ": " <>
  fromString (show (slaveOutputPaths slave))

wait :: Slave a -> IO a
wait = Async.wait . slaveExecution

waitCatch :: Slave a -> IO (Either E.SomeException a)
waitCatch = Async.waitCatch . slaveExecution

cancel :: Slave a -> IO ()
cancel = Async.cancel . slaveExecution
