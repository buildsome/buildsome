module Lib.ByteString
  ( truncateAt
  , unprefixed, unsuffixed
  , strictify, lazify
  , chopTrailingNewline, guaranteeTrailingNewline
  , splitBS
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Word

import           Prelude.Compat

chopTrailingNewline :: ByteString -> ByteString
chopTrailingNewline bs
  | "\n" `BS8.isSuffixOf` bs = BS8.init bs
  | otherwise = bs

guaranteeTrailingNewline :: ByteString -> ByteString
guaranteeTrailingNewline bs
  | "\n" `BS8.isSuffixOf` bs = bs
  | otherwise = bs <> "\n"

truncateAt :: Word8 -> ByteString -> ByteString
truncateAt z bs =
  case BS.split z bs of
  [] -> BS.empty
  (x:_) -> x

splitBS :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
splitBS small big
  | BS.null suffix = Nothing
  | otherwise = Just (prefix, BS.drop (BS.length small) suffix)
  where
    (prefix, suffix) = BS8.breakSubstring small big

unprefixed :: ByteString -> ByteString -> Maybe ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing

unsuffixed :: ByteString -> ByteString -> Maybe ByteString
unsuffixed suffix full
  | suffix `BS.isSuffixOf` full = Just $ BS.take (BS.length full - BS.length suffix) full
  | otherwise = Nothing

strictify :: BSL.ByteString -> ByteString
strictify = BS.concat . BSL.toChunks

lazify :: ByteString -> BSL.ByteString
lazify = BSL.fromChunks . (: [])
