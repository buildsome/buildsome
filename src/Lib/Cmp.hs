{-# LANGUAGE DefaultSignatures #-}
module Lib.Cmp
  ( ComparisonResult(..), Reasons
  , Cmp(..)
  , eqShow, eq
  , cmpGetter, cmpGetterBy
  ) where

import Prelude.Compat hiding (show)

import Data.ByteString (ByteString)

import Data.Monoid((<>))

import Lib.Show (show)

type Reasons = [ByteString]
data ComparisonResult reason = NotEquals reason | Equals
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup reason => Semigroup (ComparisonResult reason) where
    NotEquals x <> NotEquals y = NotEquals (x <> y)
    Equals <> x = x
    x <> Equals = x

instance Semigroup reason => Monoid (ComparisonResult reason) where
    mempty = Equals

class Cmp a where
  cmp :: a -> a -> ComparisonResult Reasons
  default cmp :: (Eq a, Show a) => a -> a -> ComparisonResult Reasons
  cmp = eqShow

eqShow :: (Eq a, Show a) => a -> a -> ComparisonResult Reasons
eqShow x y
  | x == y = Equals
  | otherwise = NotEquals [show x <> " /= " <> show y]

eq :: Eq a => reason -> a -> a -> ComparisonResult reason
eq reason x y
  | x == y = Equals
  | otherwise = NotEquals reason

cmpGetterBy ::
  (b -> b -> ComparisonResult Reasons) ->
  ByteString -> (a -> b) -> a -> a -> ComparisonResult Reasons
cmpGetterBy f str getter x y =
  map ((str <> ": ") <>) <$>
  f (getter x) (getter y)

cmpGetter :: Cmp b => ByteString -> (a -> b) -> a -> a -> ComparisonResult Reasons
cmpGetter = cmpGetterBy cmp
