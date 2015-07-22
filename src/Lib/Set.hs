{-# LANGUAGE NoImplicitPrelude #-}
module Lib.Set
  ( filterA, partitionA
  ) where



import Prelude.Compat

import Data.Set (Set)

import qualified Data.Set as S
import qualified Lib.List as L

filterA :: (Applicative f, Ord k) => (k -> f Bool) -> Set k -> f (Set k)
filterA p = fmap S.fromAscList . L.filterA p . S.toAscList

partitionA :: (Applicative f, Ord k) => (k -> f Bool) -> Set k -> f (Set k, Set k)
partitionA p = fmap mconcat . traverse onEach . S.toAscList
  where
    onEach x = partitionOne x <$> p x
    partitionOne x True  = (S.singleton x, S.empty)
    partitionOne x False = (S.empty, S.singleton x)
