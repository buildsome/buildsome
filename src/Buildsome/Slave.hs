{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes #-}
module Buildsome.Slave
  ( Slave, newWithUnmask
  , target
  , str
  , wait, waitCatch
  , cancel
  ) where

import           Prelude hiding (show, FilePath)

import qualified Buildsome.Color as Color
import           Control.Applicative ((<$>))
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import           Data.Monoid
import           Lib.ColorText (ColorText)
import           Lib.FilePath (FilePath)
import           Lib.Makefile (Target)
import qualified Lib.Printer as Printer
import           Lib.Show (show)
import           Lib.TimeInstances ()

data Slave a = Slave
  { slaveTarget :: Target
  , slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  , slaveExecution :: Async a
  }

target :: Slave a -> Target
target = slaveTarget

newWithUnmask :: Target -> Printer.Id -> [FilePath] -> ((forall b. IO b -> IO b) -> IO a) -> IO (Slave a)
newWithUnmask tgt printerId outputPaths action =
  Slave tgt printerId outputPaths <$> Async.asyncWithUnmask action

str :: Slave a -> ColorText
str slave =
  show (slavePrinterId slave) <> ": " <> cTarget (show (slaveOutputPaths slave))
  where
    Color.Scheme{..} = Color.scheme

wait :: Slave a -> IO a
wait = Async.wait . slaveExecution

waitCatch :: Slave a -> IO (Either E.SomeException a)
waitCatch = Async.waitCatch . slaveExecution

cancel :: Slave a -> IO ()
cancel = Async.cancel . slaveExecution
