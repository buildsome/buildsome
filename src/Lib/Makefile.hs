module Lib.Makefile
  ( module Lib.Makefile.Types
  , parseMakefile
  , instantiatePattern
  ) where

import Control.Monad (guard)
import Lib.Makefile.Parser
import Lib.Makefile.Types
import System.FilePath (splitFileName)
import qualified Lib.StringPattern as StringPattern
import qualified Text.Parsec as P

instantiatePattern :: FilePath -> TargetPattern -> Maybe Target
instantiatePattern outputPath (TargetPattern patDir (Target output input ooInput cmds)) = do
  guard (patDir == outputDir)
  outputMatch <- StringPattern.match output outputFile
  let plugMatch (InputPattern pat) = StringPattern.plug outputMatch pat
      plugMatch (InputPath str) = str
      pluggedInputs = map plugMatch input
      pluggedOOInputs = map plugMatch ooInput
      cmdInterpolate = interpolateString $ metaVariable [outputPath] pluggedInputs pluggedOOInputs
      interpolateMetavars = P.runParser cmdInterpolate () (show output)
      interpolatedCmds = either (error . show) id $ mapM interpolateMetavars cmds
  return $ Target [outputPath] pluggedInputs pluggedOOInputs interpolatedCmds
  where
    (outputDir, outputFile) = splitFileName outputPath
