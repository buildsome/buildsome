{-# LANGUAGE OverloadedStrings #-}
module Lib.Slave
  ( Slave, new
  , str
  , wait
  , cancel
  , wrap
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async (Async)
import Data.Monoid
import Data.String (IsString(..))
import Lib.Async (verifyCancelled)
import Lib.FilePath (FilePath)
import Lib.Printer (Printer)
import Prelude hiding (FilePath)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified Lib.Printer as Printer

data Slave = Slave
  { slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  , slaveExecution :: Async ()
  }

new :: Printer.Id -> [FilePath] -> IO () -> IO Slave
new printerId outputPaths action =
  Slave printerId outputPaths <$> Async.async action

str :: (Monoid str, IsString str) => Slave -> str
str slave =
  Printer.idStr (slavePrinterId slave) <> ": " <>
  fromString (show (slaveOutputPaths slave))

wait :: Printer -> Slave -> IO ()
wait _printer slave =
  -- Keep this around so we can enable logging about slave waits
  -- easily:
  -- Printer.printWrap _printer ("Waiting for " <> str slave) $
  Async.wait $ slaveExecution slave

cancel :: Slave -> IO (Either E.SomeException ())
cancel = verifyCancelled . slaveExecution

wrap :: (IO () -> IO ()) -> Slave -> IO Slave
wrap f slave = do
  wrappedExecution <- (Async.async . f . Async.wait . slaveExecution) slave
  return slave { slaveExecution = wrappedExecution }
