module Lib.Argv0 (getArgv0) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Filesystem.Path.CurrentOS (encode)
import qualified System.Argv0 as Argv0

getArgv0 :: IO ByteString
getArgv0 = encode <$> Argv0.getArgv0
