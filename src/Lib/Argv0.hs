module Lib.Argv0 (getArgv0) where

import Control.Applicative ((<$>))
import Filesystem.Path.CurrentOS (encodeString)
import qualified System.Argv0 as Argv0

getArgv0 :: IO String
getArgv0 = encodeString <$> Argv0.getArgv0
