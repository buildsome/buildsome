module Lib.Binary (runGet, runPut, encode, decode) where

import           Data.Binary (Binary, Get, Put)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import           Lib.ByteString (strictify, lazify)

import           Prelude.Compat

{-# INLINE runGet #-}
runGet :: Get a -> BS.ByteString -> a
runGet x = Get.runGet x . lazify

{-# INLINE runPut #-}
runPut :: Put -> BS.ByteString
runPut = strictify . Put.runPut

{-# INLINE decode #-}
decode :: Binary a => BS.ByteString -> a
decode = Binary.decode . lazify

{-# INLINE encode #-}
encode :: Binary a => a -> BS.ByteString
encode = strictify . Binary.encode
