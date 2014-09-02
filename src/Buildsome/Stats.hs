module Buildsome.Stats
  ( When(..), TargetStats(..), Stats(..)
  ) where

import Prelude hiding (FilePath)

import Buildsome.BuildMaps (TargetRep)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Time (DiffTime)
import Lib.FilePath (FilePath)
import Lib.Makefile (Target)

data When = FromCache | BuiltNow deriving Show

data TargetStats = TargetStats
  { tsWhen :: !When
  , tsTime :: !DiffTime
  , tsDirectDeps :: [Target]
  , tsAccessedPaths :: [FilePath]
  } deriving (Show)

data Stats = Stats
  { ofTarget :: Map TargetRep TargetStats
  , stdErr :: Set TargetRep
  } deriving (Show)

instance Monoid Stats where
  mempty = Stats mempty mempty
  mappend (Stats a1 a2) (Stats b1 b2) =
    Stats
    (a1 `mappend` b1)
    (a2 `mappend` b2)
