{-# LANGUAGE NoImplicitPrelude #-}
module Lib.List (filterA, unprefixed, unsuffixed, partitionA, partitionA') where

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

partitionA :: (Applicative f) => (a -> f Bool) -> [a] -> f ([a], [a])
partitionA = partitionA' (:[]) []

partitionA' ::
  (Applicative f, Monoid t) =>
  (t1 -> t) -> t -> (t1 -> f Bool) -> [t1] -> f (t, t)
partitionA' singleton' empty' p = fmap mconcat . traverse onEach
  where
    onEach x = partitionOne x <$> p x
    partitionOne x True  = (singleton' x, empty')
    partitionOne x False = (empty', singleton' x)

unprefixed :: Eq a => [a] -> [a] -> Maybe [a]
unprefixed prefix full
  | prefix `isPrefixOf` full = Just $ drop (length prefix) full
  | otherwise = Nothing

unsuffixed :: Eq a => [a] -> [a] -> Maybe [a]
unsuffixed suffix full
  | suffix `isSuffixOf` full = Just $ take (length full - length suffix) full
  | otherwise = Nothing

