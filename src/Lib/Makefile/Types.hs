module Lib.Makefile.Types
  ( TargetType(..)
  , FilePattern(..), onFilePatternPaths
  , InputPat(..), onInputPatPaths
  , Target, onTargetPaths
  , Pattern, onPatternPaths
  , Makefile(..), onMakefilePaths
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Traversable (traverse)
import Lib.StringPattern (StringPattern)

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: [String]
  } deriving (Show)

type Target = TargetType FilePath FilePath

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show)

data InputPat = InputPath FilePath | InputPattern FilePattern
  deriving (Show)

type Pattern = TargetType FilePattern InputPat

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [FilePath]
  } deriving (Show)

-- Filepath lens boilerplate:

onTargetPaths :: Applicative f => (FilePath -> f FilePath) -> Target -> f Target
onTargetPaths f (Target outputs inputs orderOnlyInputs cmds) =
  Target
  <$> traverse f outputs
  <*> traverse f inputs
  <*> traverse f orderOnlyInputs
  <*> pure cmds

onFilePatternPaths :: Functor f => (FilePath -> f FilePath) -> (FilePattern -> f FilePattern)
onFilePatternPaths f (FilePattern dir file) = (`FilePattern` file) <$> f dir

onInputPatPaths :: Functor f => (FilePath -> f FilePath) -> InputPat -> f InputPat
onInputPatPaths f (InputPath x) = InputPath <$> f x
onInputPatPaths f (InputPattern x) = InputPattern <$> onFilePatternPaths f x

onPatternPaths :: Applicative f => (FilePath -> f FilePath) -> Pattern -> f Pattern
onPatternPaths f (Target outputs inputs orderOnlyInputs cmds) =
  Target
  <$> traverse (onFilePatternPaths f) outputs
  <*> traverse (onInputPatPaths f) inputs
  <*> traverse (onInputPatPaths f) orderOnlyInputs
  <*> pure cmds

onMakefilePaths :: Applicative f => (FilePath -> f FilePath) -> Makefile -> f Makefile
onMakefilePaths f (Makefile targets patterns phonies) =
  Makefile
  <$> traverse (onTargetPaths f) targets
  <*> traverse (onPatternPaths f) patterns
  <*> traverse f phonies
