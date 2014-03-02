module Lib.Makefile.InstantiatePattern
  ( instantiatePatternByOutput
  , instantiatePatternByMatch
  ) where

import Control.Monad (guard)
import Lib.FilePath (splitFileName)
import Lib.Makefile.Parser (interpolateCmds)
import Lib.Makefile.Types
import Lib.StringPattern (matchPlaceHolder)
import System.FilePath ((</>))
import qualified Lib.StringPattern as StringPattern

instantiatePatternWith :: FilePath -> StringPattern.Match -> TargetPattern -> Target
instantiatePatternWith outputPath match (TargetPattern patOutDir (Target _ input ooInput cmds)) =
  interpolateCmds mStem $
  Target [outputPath] pluggedInputs pluggedOOInputs cmds
  where
    mStem = Just (patOutDir </> matchPlaceHolder match)
    plugMatch (InputPattern pat) = StringPattern.plug match pat
    plugMatch (InputPath str) = str
    pluggedInputs = map plugMatch input
    pluggedOOInputs = map plugMatch ooInput

instantiatePatternByOutput :: FilePath -> TargetPattern -> Maybe Target
instantiatePatternByOutput outputPath pat@(TargetPattern patOutDir target) = do
  guard (patOutDir == outputDir)
  outputMatch <- StringPattern.match (targetOutput target) outputFile
  return $ instantiatePatternWith outputPath outputMatch pat
  where
    (outputDir, outputFile) = splitFileName outputPath

instantiatePatternByMatch :: StringPattern.Match -> TargetPattern -> Target
instantiatePatternByMatch match pat@(TargetPattern patOutDir target) =
  instantiatePatternWith outputPath match pat
  where
    outputPath = patOutDir </> StringPattern.plug match (targetOutput target)
