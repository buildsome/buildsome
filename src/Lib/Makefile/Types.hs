module Lib.Makefile.Types
  ( TargetType(..)
  , Target, InputPat(..), TargetPattern(..)
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

data InputPat = InputPath String | InputPattern StringPattern
  deriving (Show)

data TargetPattern = TargetPattern
  { targetPatternOutputDirectory :: FilePath

-- TODO: Don't abuse a shared TargetType here? Just duplicate the 4 fields?

    -- The output pattern in the directory (We allow only 1 output and
    -- the pattern must be in the file component). The input patterns
    -- are more flexible and can be full paths
  , targetPatternTarget :: TargetType StringPattern InputPat
  } deriving (Show)

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefileTargetPatterns :: [TargetPattern]
  , makefilePhonies :: [FilePath]
  } deriving (Show)
