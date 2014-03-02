module Lib.Makefile
  ( module Lib.Makefile.Types
  , parseMakefile
  , instantiatePatternByOutput
  , instantiatePatternByMatch
  ) where

import Control.Monad (guard)
import Lib.FilePath (splitFileName)
import Lib.Makefile.Parser
import Lib.Makefile.Types
import Lib.StringPattern (matchPlaceHolder)
import System.FilePath ((</>))
import qualified Lib.StringPattern as StringPattern
import qualified Text.Parsec as P

instantiatePatternWith :: FilePath -> StringPattern.Match -> TargetPattern -> Target
instantiatePatternWith outputPath match (TargetPattern patOutDir (Target output input ooInput cmds)) =
  Target [outputPath] pluggedInputs pluggedOOInputs interpolatedCmds
  where
    plugMatch (InputPattern pat) = StringPattern.plug match pat
    plugMatch (InputPath str) = str
    pluggedInputs = map plugMatch input
    pluggedOOInputs = map plugMatch ooInput
    cmdInterpolate =
      interpolateString $
      metaVariable [outputPath] pluggedInputs pluggedOOInputs $
      Just (patOutDir </> matchPlaceHolder match)
    interpolateMetavars = P.runParser cmdInterpolate () (show output)
    interpolatedCmds = either (error . show) id $ mapM interpolateMetavars cmds

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
