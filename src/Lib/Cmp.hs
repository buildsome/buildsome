{-# LANGUAGE DefaultSignatures, DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}
module Lib.Cmp
  ( ComparisonResult(..), Reasons
  , Cmp(..)
  , eqShow, eq
  , cmpGetter, cmpGetterBy
  ) where

import Prelude hiding (show)

import Data.ByteString (ByteString)
import Data.Foldable (Foldable)
import Data.Monoid(Monoid(..), (<>))
import Data.Traversable (Traversable)
import Lib.Show (show)

type Reasons = [ByteString]
data ComparisonResult reason = NotEquals reason | Equals
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Monoid reason => Monoid (ComparisonResult reason) where
  mempty = Equals
  mappend (NotEquals x) (NotEquals y) = NotEquals (mappend x y)
  mappend Equals x = x
  mappend x Equals = x

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
  fmap (map ((str <> ": ") <>)) $
  f (getter x) (getter y)

cmpGetter :: Cmp b => ByteString -> (a -> b) -> a -> a -> ComparisonResult Reasons
cmpGetter = cmpGetterBy cmp
