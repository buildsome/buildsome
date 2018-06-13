module Lib.Makefile.MonadClass (MonadMakefileParser(..)) where

import Prelude.Compat hiding (FilePath)

import Data.ByteString (ByteString)
import Lib.FilePath (FilePath)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified System.IO as IO

class Monad m => MonadMakefileParser m where
  outPutStrLn :: ByteString -> m ()
  errPutStrLn :: ByteString -> m ()
  tryReadFile :: FilePath -> m (Either E.SomeException ByteString)

instance MonadMakefileParser IO where
  outPutStrLn = BS8.putStrLn
  errPutStrLn = BS8.hPutStrLn IO.stderr
  tryReadFile = E.try . BS8.readFile . BS8.unpack
