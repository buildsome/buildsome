{-# LANGUAGE OverloadedStrings #-}
module Lib.ByteString
  ( truncateAt
  , unprefixed, unsuffixed, unsuffixedWildCard
  , strictify, lazify
  , chopTrailingNewline, guaranteeTrailingNewline
  ) where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

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

breakSubstringR :: ByteString -> ByteString -> (ByteString, ByteString)
-- Is this faster? It's definitely simpler!
-- breakSubstringR x = BS.reverse . BS.breakSubstring (BS.reverse x) . BS.reverse
breakSubstringR needle haystack =
    case BS.breakSubstring needle haystack of
      (x, "") -> (x, "")
      (pre, suf) ->
          case BS.breakSubstring needle $ BS.drop (BS.length needle) suf of
            (_, "") -> (pre, suf)
            (subpre, subsuf) -> (BS.concat [pre, needle, subpre], subsuf)

unprefixed :: ByteString -> ByteString -> Maybe ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing

unsuffixed :: ByteString -> ByteString -> Maybe ByteString
unsuffixed suffix full
  | suffix `BS.isSuffixOf` full = Just $ BS.take (BS.length full - BS.length suffix) full
  | otherwise = Nothing

unsuffixedWildCard :: ByteString -> ByteString -> (Maybe ByteString, Maybe ByteString)
unsuffixedWildCard suffix full =
    case BS.breakSubstring "*" suffix of
      (_suffix, "") -> (unsuffixed suffix full, Nothing)
      (prewc, withwc) ->
        case unsuffixed (BS.drop 1 withwc) full of
          Nothing -> (Nothing, Nothing)
          Just m -> case breakSubstringR prewc m of
                (_, "") -> (Nothing, Nothing)
                (a, b) -> (Just a, Just $ BS.drop (BS.length prewc) b)

strictify :: BSL.ByteString -> ByteString
strictify = BS.concat . BSL.toChunks

lazify :: ByteString -> BSL.ByteString
lazify = BSL.fromChunks . (: [])
