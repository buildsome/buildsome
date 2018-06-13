module Lib.Argv0 (getArgv0) where


import Prelude.Compat

import Data.ByteString (ByteString)
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.ByteString.Char8 as BS8
import qualified System.Argv0 as Argv0

getArgv0 :: IO ByteString
getArgv0 = BS8.pack . encodeString <$> Argv0.getArgv0
