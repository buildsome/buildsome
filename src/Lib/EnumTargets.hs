module Lib.EnumTargets
  ( enumTargetsFromPattern
  ) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Lib.Makefile (TargetPattern(..), TargetType(..), Target, InputPat(..), instantiatePatternByMatch)
import Lib.StringPattern (StringPattern(..))
import System.FilePath.Glob (glob)
import qualified Lib.StringPattern as StringPattern

warning :: String
warning = unlines
  [ "WARNING: Unimplemented recursive descent into input "
  , "directories may result in incorrect build results!" ]

getMatches :: StringPattern -> IO [StringPattern.Match]
getMatches pat@(StringPattern prefix suffix) = do
  paths <- glob (prefix ++ "*" ++ suffix)
  return $
    map
    (fromMaybe (error "glob matched but StringPattern doesn't?!") .
     StringPattern.match pat) paths

-- TODO: This goes 1 level deep, doesn't correctly recursively need
enumTargetsFromPattern :: TargetPattern -> IO [Target]
enumTargetsFromPattern targetPattern = do
  putStr warning
  -- TODO: Here is where we'd recursively makeSlaves (not "need"
  -- because our caller can parallelize this)
  inputMatches <- concat <$> mapM getMatches inputPats
  return $ map (`instantiatePatternByMatch` targetPattern) inputMatches
  where
    target = targetPatternTarget targetPattern
    inputPats =
      [ strPat
      | InputPattern strPat <-
        targetInput target ++ targetOrderOnlyInput target
      ]
