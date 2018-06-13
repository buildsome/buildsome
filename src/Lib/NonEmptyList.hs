module Lib.NonEmptyList
       ( NonEmptyList(..)
       , head
       , tail
       , singleton
       , cons
       , lookup
       , fromHeadAndTail
       )
       where

import           Data.Binary    (Binary (..))
import           GHC.Generics   (Generic)
import           Prelude.Compat hiding (head, lookup, tail)

data NonEmptyList a =
  NonEmptyList
  { neHead :: a
  , neTail :: [a]
  }
  deriving (Show, Eq, Generic, Functor, Traversable, Foldable)
instance Binary a => Binary (NonEmptyList a)

head :: NonEmptyList a -> a
head = neHead

tail :: NonEmptyList a -> [a]
tail = neTail

singleton :: a -> NonEmptyList a
singleton x = NonEmptyList x []

cons :: a -> NonEmptyList a -> NonEmptyList a
cons x (NonEmptyList y ys) = NonEmptyList x (y:ys)

lookup :: Eq a => a -> NonEmptyList (a, b) -> Maybe b
lookup x (NonEmptyList (y,b) ys) = go x ((y, b):ys)
  where
    go _  [] = Nothing
    go x' ((y', b'):ys')
      | x' == y'  = Just b'
      | otherwise = go x' ys'

fromHeadAndTail :: a -> [a] -> NonEmptyList a
fromHeadAndTail = NonEmptyList
