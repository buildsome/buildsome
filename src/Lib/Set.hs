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
partitionA p = L.partitionA' S.singleton S.empty p . S.toAscList
