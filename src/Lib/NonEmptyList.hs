{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.NonEmptyList where

import           Prelude.Compat
import           Data.Binary             (Binary (..))
import           GHC.Generics            (Generic)

data NonEmptyList a =
  NonEmptyList
  { neHead :: a
  , neTail :: [a]
  }
  deriving (Show, Eq, Generic, Functor, Traversable, Foldable)
instance Binary a => Binary (NonEmptyList a)

singleton :: a -> NonEmptyList a
singleton x = NonEmptyList x []

cons :: a -> NonEmptyList a -> NonEmptyList a
cons x (NonEmptyList y ys) = NonEmptyList x (y:ys)
