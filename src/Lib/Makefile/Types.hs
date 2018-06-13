module Lib.Makefile.Types
  ( TargetType(..), targetAllInputs
  , FilePattern(..), onFilePatternPaths
  , InputPat(..), onInputPatPaths
  , Target, onTargetPaths, targetInterpolatedCmds
  , Pattern, onPatternPaths
  , VarName, VarValue, Vars
  , Makefile(..), onMakefilePaths
  ) where


import Prelude.Compat hiding (FilePath)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)
import BMake.Data

import GHC.Generics (Generic)
import Lib.FilePath (FilePath)
import Lib.Parsec () -- instance Binary SourcePos
import Lib.StringPattern (StringPattern)
import qualified Text.Parsec.Pos as ParsecPos

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: Either ByteString [[Expr3]] -- ToDo: Should be [Expr3] only
                                              -- it's transitory.
  , targetPos :: ParsecPos.SourcePos
  } deriving (Show, Generic)
instance (Binary output, Binary input) => Binary (TargetType output input)
instance (NFData output, NFData input) => NFData (TargetType output input) where
  rnf = genericRnf

type Target = TargetType FilePath FilePath

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show, Generic)
instance Binary FilePattern
instance NFData FilePattern where rnf = genericRnf

data InputPat = InputPath FilePath | InputPattern FilePattern
  deriving (Show, Generic)
instance Binary InputPat
instance NFData InputPat where rnf = genericRnf

type Pattern = TargetType FilePattern InputPat

type VarName = ByteString
type VarValue = ByteString
type Vars = Map VarName VarValue

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [(ParsecPos.SourcePos, FilePath)]
  , makefileWeakVars :: Vars
  } deriving (Show, Generic)
instance Binary Makefile
instance NFData Makefile where rnf = genericRnf

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

targetInterpolatedCmds :: Target -> ByteString
targetInterpolatedCmds target =
    case targetCmds target of
        Left x -> x
        Right _ -> error "target was not interpolated"

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

_2 :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
_2 f (x, y) = (,) x <$> f y

onMakefilePaths :: Applicative f => (FilePath -> f FilePath) -> Makefile -> f Makefile
onMakefilePaths f (Makefile targets patterns phonies weakVars) =
  Makefile
  <$> traverse (onTargetPaths f) targets
  <*> traverse (onPatternPaths f) patterns
  <*> (traverse . _2) f phonies
  <*> pure weakVars
