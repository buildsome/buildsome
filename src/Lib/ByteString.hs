module Lib.ByteString
  ( truncateAt
  , unprefixed, unsuffixed
  , strictify, lazify
  ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

truncateAt :: Word8 -> BS.ByteString -> BS.ByteString
truncateAt z bs =
  case BS.split z bs of
  [] -> BS.empty
  (x:_) -> x

unprefixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing

unsuffixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unsuffixed suffix full
  | suffix `BS.isSuffixOf` full = Just $ BS.take (BS.length full - BS.length suffix) full
  | otherwise = Nothing

strictify :: BSL.ByteString -> BS.ByteString
strictify = BS.concat . BSL.toChunks

lazify :: BS.ByteString -> BSL.ByteString
lazify = BSL.fromChunks . (: [])
