module Lib.Makefile.Types
  ( TargetType(..)
  , FilePattern(..), InputPat(..)
  , Target, Pattern
  , Makefile(..)
  ) where

import Lib.StringPattern (StringPattern)

type Target = TargetType [FilePath] FilePath

data TargetType outputs input = Target
  { targetOutput :: outputs
  , targetInput :: [input]
  , targetOrderOnlyInput :: [input]
  , targetCmds :: [String]
  } deriving (Show)

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show)

data InputPat = InputPath String | InputPattern StringPattern
  deriving (Show)

type Pattern = TargetType FilePattern InputPat

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [FilePath]
  } deriving (Show)
