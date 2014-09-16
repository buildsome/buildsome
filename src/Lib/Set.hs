module Lib.Set
  ( filterA
  ) where

import Control.Applicative (Applicative)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Lib.List as L

filterA :: (Applicative f, Ord k) => (k -> f Bool) -> Set k -> f (Set k)
filterA p = fmap S.fromAscList . L.filterA p . S.toAscList
