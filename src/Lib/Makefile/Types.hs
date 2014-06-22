module Lib.Makefile.Types
  ( TargetType(..), targetAllInputs
  , FilePattern(..), onFilePatternPaths
  , InputPat(..), onInputPatPaths
  , Target, onTargetPaths
  , Pattern, onPatternPaths
  , VarName, VarValue, Vars
  , Makefile(..), onMakefilePaths
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Traversable (traverse)
import Lib.FilePath (FilePath)
import Lib.StringPattern (StringPattern)
import Prelude hiding (FilePath)
import qualified Text.Parsec as Parsec

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: ByteString
  , targetPos :: Parsec.SourcePos
  } deriving (Show)

type Target = TargetType FilePath FilePath

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show)

data InputPat = InputPath FilePath | InputPattern FilePattern
  deriving (Show)

type Pattern = TargetType FilePattern InputPat

type VarName = ByteString
type VarValue = ByteString
type Vars = Map VarName VarValue

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [FilePath]
  , makefileWeakVars :: Vars
  } deriving (Show)

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

-- Filepath lens boilerplate:

onTargetPaths :: Applicative f => (FilePath -> f FilePath) -> Target -> f Target
onTargetPaths f (Target outputs inputs orderOnlyInputs cmds pos) =
  Target
  <$> traverse f outputs
  <*> traverse f inputs
  <*> traverse f orderOnlyInputs
  <*> pure cmds
  <*> pure pos

onFilePatternPaths :: Functor f => (FilePath -> f FilePath) -> FilePattern -> f FilePattern
onFilePatternPaths f (FilePattern dir file) = (`FilePattern` file) <$> f dir

onInputPatPaths :: Functor f => (FilePath -> f FilePath) -> InputPat -> f InputPat
onInputPatPaths f (InputPath x) = InputPath <$> f x
onInputPatPaths f (InputPattern x) = InputPattern <$> onFilePatternPaths f x

onPatternPaths :: Applicative f => (FilePath -> f FilePath) -> Pattern -> f Pattern
onPatternPaths f (Target outputs inputs orderOnlyInputs cmds pos) =
  Target
  <$> traverse (onFilePatternPaths f) outputs
  <*> traverse (onInputPatPaths f) inputs
  <*> traverse (onInputPatPaths f) orderOnlyInputs
  <*> pure cmds
  <*> pure pos

onMakefilePaths :: Applicative f => (FilePath -> f FilePath) -> Makefile -> f Makefile
onMakefilePaths f (Makefile targets patterns phonies weakVars) =
  Makefile
  <$> traverse (onTargetPaths f) targets
  <*> traverse (onPatternPaths f) patterns
  <*> traverse f phonies
  <*> pure weakVars
