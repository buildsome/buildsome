module Lib.ByteString (truncateAt, unprefixed) where

import Data.Word
import qualified Data.ByteString as BS

truncateAt :: Word8 -> BS.ByteString -> BS.ByteString
truncateAt z bs =
  case BS.split z bs of
  [] -> BS.empty
  (x:_) -> x

unprefixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing
