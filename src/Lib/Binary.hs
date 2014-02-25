module Lib.Binary (runGet, runPut) where

import Data.Binary (Get, Put)
import Lib.ByteString (strictify, lazify)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS

runGet :: Get a -> BS.ByteString -> a
runGet x = Get.runGet x . lazify

runPut :: Put -> BS.ByteString
runPut = strictify . Put.runPut