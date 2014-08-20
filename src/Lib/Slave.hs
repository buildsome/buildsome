{-# LANGUAGE OverloadedStrings #-}
module Lib.Slave
  ( Slave, new
  , target
  , str
  , wait, waitCatch
  , cancel
  , When(..), Stats(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Lib.BuildMaps (TargetRep)
import Lib.FilePath (FilePath)
import Lib.Makefile (Target)
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified Lib.Printer as Printer

-- TODO: Get this Stats business out of here and have "Slave a"?
data When = FromCache | BuiltNow deriving Show

data Stats = Stats
  { statsOfTarget :: Map TargetRep (When, DiffTime, [Target])
  , statsStdErr :: Set TargetRep
  } deriving (Show)

instance Monoid Stats where
  mempty = Stats mempty mempty
  mappend (Stats a1 a2) (Stats b1 b2) =
    Stats
    (a1 `mappend` b1)
    (a2 `mappend` b2)

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
