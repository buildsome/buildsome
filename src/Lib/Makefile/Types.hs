module Lib.Makefile.Types
  ( TargetType(..)
  , FilePattern(..), InputPat(..)
  , Target, Pattern
  , Makefile(..)
  ) where

import Lib.StringPattern (StringPattern)

type Target = TargetType FilePath FilePath

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: [String]
  } deriving (Show)

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show)

data InputPat = InputPath String | InputPattern FilePattern
  deriving (Show)

type Pattern = TargetType FilePattern InputPat

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [FilePath]
  } deriving (Show)
