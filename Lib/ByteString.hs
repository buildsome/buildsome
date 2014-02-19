module Lib.ByteString (unprefixed) where

import qualified Data.ByteString as BS

unprefixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing
