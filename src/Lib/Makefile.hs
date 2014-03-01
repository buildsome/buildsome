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

instantiatePattern :: TargetPattern -> FilePath -> Maybe Target
instantiatePattern (TargetPattern patDir (Target output input ooInput cmds)) outputPath = do
  guard (patDir == outputDir)
  outputMatch <- StringPattern.match output outputFile
  let plugMatch (InputPattern pat) = StringPattern.plug outputMatch pat
      plugMatch (InputPath str) = str
  return $ Target [outputPath] (map plugMatch input) (map plugMatch ooInput) cmds
  where
    (outputDir, outputFile) = splitFileName outputPath
