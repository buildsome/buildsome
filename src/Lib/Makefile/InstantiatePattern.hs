module Lib.Makefile.InstantiatePattern
  ( instantiatePatternByOutput
  , instantiatePatternByMatch
  ) where

import Control.Monad (guard, msum)
import Lib.FilePath ((</>), FilePath, splitFileName)
import Lib.Makefile.Parser (interpolateCmds)
import Lib.Makefile.Types
import Lib.StringPattern (matchPlaceHolder)
import Prelude hiding (FilePath)
import qualified Lib.StringPattern as StringPattern

plugFilePattern :: StringPattern.Match -> FilePattern -> FilePath
plugFilePattern match (FilePattern dir file) = dir </> StringPattern.plug match file

instantiatePatternByMatch :: StringPattern.Match -> Pattern -> Target
instantiatePatternByMatch match (Target outputs inputs ooInputs cmds) =
  interpolateCmds mStem $
  Target pluggedOutputs pluggedInputs pluggedOOInputs cmds
  where
    mStem = Just (matchPlaceHolder match)
    plugInputMatch (InputPattern pat) = plugFilePattern match pat
    plugInputMatch (InputPath str) = str
    pluggedOutputs  = map (plugFilePattern match) outputs
    pluggedInputs   = map plugInputMatch inputs
    pluggedOOInputs = map plugInputMatch ooInputs

instantiatePatternByOutput :: FilePath -> Pattern -> Maybe Target
instantiatePatternByOutput outputPath target =
  msum $ map tryMatchOutput (targetOutputs target)
  where
    (outputDir, outputFile) = splitFileName outputPath
    tryMatchOutput (FilePattern patDir patFile) = do
      guard (patDir == outputDir)
      outputMatch <- StringPattern.match patFile outputFile
      return $ instantiatePatternByMatch outputMatch target
