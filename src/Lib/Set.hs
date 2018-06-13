module Lib.Set
  ( filterA, partitionA
  ) where

import           Data.Set (Set)
import qualified Data.Set as S
import qualified Lib.List as L

import           Prelude.Compat

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

filterA :: (Applicative f, Ord k) => (k -> f Bool) -> Set k -> f (Set k)
filterA p = fmap S.fromAscList . L.filterA p . S.toAscList

partitionA :: (Applicative f, Ord k) => (k -> f Bool) -> Set k -> f (Set k, Set k)
partitionA p = fmap (both S.fromAscList) . L.partitionA p . S.toAscList
