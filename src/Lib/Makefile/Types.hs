module Lib.Makefile.Types
  ( Target(..)
  , Makefile(..)
  ) where

data Target = Target
  { targetOutputPaths :: [FilePath]
  , targetExplicitInputHints :: [FilePath]
  , targetOrderOnlyInputHints :: [FilePath]
  , targetCmds :: [String]
  } deriving (Show)

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePhonies :: [FilePath]
  } deriving (Show)
