module Buildsome.Stats
  ( When(..), TargetStats(..), Stats(..)
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.BuildMaps (TargetRep)
import Data.Map (Map)

import Data.Set (Set)
import Data.Time (DiffTime)
import Lib.Makefile (Target)
import Lib.FilePath (FilePath)

data When = FromCache | BuiltNow deriving Show

data TargetStats = TargetStats
  { tsWhen :: !When
    -- | How long it took to run
  , tsTime :: !DiffTime
  , tsDirectDeps :: [Target]
  , tsExistingInputs :: Maybe [FilePath]
  } deriving (Show)

data Stats = Stats
  { ofTarget :: Map TargetRep TargetStats
  , stdErr :: Set TargetRep
  } deriving (Show)

instance Semigroup Stats where
    Stats a1 a2 <> Stats b1 b2 = Stats (a1 <> b1) (a2 <> b2)
instance Monoid Stats where
    mempty = Stats mempty mempty
