module Buildsome.Stats
  ( When(..), Stats(..)
  ) where

import Buildsome.BuildMaps (TargetRep)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Time (DiffTime)
import Lib.Makefile (Target)

data When = FromCache | BuiltNow deriving Show

data Stats = Stats
  { ofTarget :: Map TargetRep (When, DiffTime, [Target])
  , stdErr :: Set TargetRep
  } deriving (Show)

instance Monoid Stats where
  mempty = Stats mempty mempty
  mappend (Stats a1 a2) (Stats b1 b2) =
    Stats
    (a1 `mappend` b1)
    (a2 `mappend` b2)
