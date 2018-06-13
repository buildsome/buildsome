module Lib.Map
  ( filterA, filterAWithKey
  ) where

import           Prelude.Compat hiding (FilePath)

import Data.Map (Map)
import qualified Data.Map as Map

filterA :: Applicative f => (a -> f Bool) -> Map k a -> f (Map k a)
filterA = filterAWithKey . const

filterAWithKey :: Applicative f => (k -> a -> f Bool) -> Map k a -> f (Map k a)
filterAWithKey predicate =
  fmap (Map.mapMaybe removeFalse) .
  Map.traverseWithKey annotatePredicate
  where
    annotatePredicate k x = (,) x <$> predicate k x
    removeFalse (_, False) = Nothing
    removeFalse (x, True) = Just x
