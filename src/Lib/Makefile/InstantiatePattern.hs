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

plugFilePattern :: StringPattern.Match -> FilePattern -> FilePath
plugFilePattern match (FilePattern dir file) = dir </> StringPattern.plug match file

instantiatePatternWith :: FilePath -> StringPattern.Match -> Pattern -> Target
instantiatePatternWith outputPath match (Target output input ooInput cmds) =
  interpolateCmds mStem $
  Target [outputPath] pluggedInputs pluggedOOInputs cmds
  where
    patDir = filePatternDirectory output
    mStem = Just (patDir </> matchPlaceHolder match)
    plugMatch (InputPattern pat) = plugFilePattern match pat
    plugMatch (InputPath str) = str
    pluggedInputs = map plugMatch input
    pluggedOOInputs = map plugMatch ooInput

instantiatePatternByOutput :: FilePath -> Pattern -> Maybe Target
instantiatePatternByOutput outputPath target = do
  guard (patDir == outputDir)
  outputMatch <- StringPattern.match patFile outputFile
  return $ instantiatePatternWith outputPath outputMatch target
  where
    FilePattern patDir patFile = targetOutput target
    (outputDir, outputFile) = splitFileName outputPath

instantiatePatternByMatch :: StringPattern.Match -> Pattern -> Target
instantiatePatternByMatch match target =
  instantiatePatternWith outputPath match target
  where
    FilePattern patDir patFile = targetOutput target
    outputPath = patDir </> StringPattern.plug match patFile
