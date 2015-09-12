{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Lib.NonEmptyMap
  ( NonEmptyMap
  , singleton
  , insert
  , lookup
  , toMap
  , toList
  , toNonEmptyList
  )
  where

import           Data.Binary    (Binary)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           GHC.Generics   (Generic)
-- REVIEW(Eyal): Why the space?
import           Lib.NonEmptyList (NonEmptyList(..))
import           Prelude.Compat hiding (lookup)

newtype NonEmptyMap k v = NonEmptyMap (Map k v)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (Binary k, Binary v) => Binary (NonEmptyMap k v)

singleton :: k -> v -> NonEmptyMap k v
singleton k = NonEmptyMap . Map.singleton k

insert :: Ord k => k -> a -> NonEmptyMap k a -> NonEmptyMap k a
insert k v (NonEmptyMap m) = NonEmptyMap $ Map.insert k v m

lookup :: Ord k => k -> NonEmptyMap k a -> Maybe a
lookup k (NonEmptyMap m) = Map.lookup k m

toMap :: NonEmptyMap k v -> Map k v
toMap (NonEmptyMap m) = m

toList :: NonEmptyMap k v -> [(k, v)]
toList = Map.toList . toMap

toNonEmptyList :: NonEmptyMap k v -> NonEmptyList (k, v)
toNonEmptyList nem = NonEmptyList (head l) (tail l)
  where l = toList nem
