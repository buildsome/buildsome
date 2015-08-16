{-# LANGUAGE NoImplicitPrelude #-}
module Lib.List (filterA, unprefixed, unsuffixed) where

import Prelude.Compat

import Data.List (isPrefixOf, isSuffixOf)

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA p = go
  where
    go [] = pure []
    go (x:xs) = combine <$> p x <*> go xs
      where
        combine True rest = x : rest
        combine False rest = rest

unprefixed :: Eq a => [a] -> [a] -> Maybe [a]
unprefixed prefix full
  | prefix `isPrefixOf` full = Just $ drop (length prefix) full
  | otherwise = Nothing

unsuffixed :: Eq a => [a] -> [a] -> Maybe [a]
unsuffixed suffix full
  | suffix `isSuffixOf` full = Just $ take (length full - length suffix) full
  | otherwise = Nothing
