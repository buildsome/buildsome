module Lib.List (unprefixed, unsuffixed) where

import Data.List (isPrefixOf, isSuffixOf)

unprefixed :: Eq a => [a] -> [a] -> Maybe [a]
unprefixed prefix full
  | prefix `isPrefixOf` full = Just $ drop (length prefix) full
  | otherwise = Nothing

unsuffixed :: Eq a => [a] -> [a] -> Maybe [a]
unsuffixed suffix full
  | suffix `isSuffixOf` full = Just $ take (length full - length suffix) full
  | otherwise = Nothing
