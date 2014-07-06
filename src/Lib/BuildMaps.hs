{-# LANGUAGE OverloadedStrings #-}
module Lib.BuildMaps
  ( TargetRep(..)
  , DirectoryBuildMap(..)
  , BuildMaps(..)
  , make, find
  , findDirectory
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Lib.FilePath (FilePath, takeDirectory)
import Lib.Makefile (Makefile(..), TargetType(..), Target, Pattern)
import Prelude hiding (FilePath)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Lib.Makefile as Makefile
import qualified Lib.StringPattern as StringPattern

newtype TargetRep = TargetRep { targetRepPath :: FilePath } -- We use the minimum output path as the target key/representative
  deriving (Eq, Ord, Show)
computeTargetRep :: Target -> TargetRep
computeTargetRep = TargetRep . minimum . targetOutputs

data DirectoryBuildMap = DirectoryBuildMap
  { dbmTargets :: [(TargetRep, Target)]
  , dbmPatterns :: [Pattern]
  } deriving (Show)
instance Monoid DirectoryBuildMap where
  mempty = DirectoryBuildMap mempty mempty
  mappend (DirectoryBuildMap x0 x1) (DirectoryBuildMap y0 y1) =
    DirectoryBuildMap (mappend x0 y0) (mappend x1 y1)

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath (TargetRep, Target) -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath DirectoryBuildMap
  }

find :: BuildMaps -> FilePath -> Maybe (TargetRep, Target)
find (BuildMaps buildMap childrenMap) path =
  -- Allow specific/direct matches to override pattern matches
  directMatch `mplus` patternMatch
  where
    directMatch = path `M.lookup` buildMap
    patterns = dbmPatterns $ M.findWithDefault mempty (takeDirectory path) childrenMap
    instantiate pattern = (,) pattern <$> Makefile.instantiatePatternByOutput path pattern
    patternMatch =
      case mapMaybe instantiate patterns of
      [] -> Nothing
      [(_, target)] -> Just (computeTargetRep target, target)
      targets ->
        error $ BS8.unpack $ mconcat
        [ "Multiple matching patterns for: ", BS8.pack (show path), "\n"
        , BS8.unlines $ map BS8.unwords $
          map
          ( map (StringPattern.toString . Makefile.filePatternFile) .
            targetOutputs . fst
          )
          targets
        ]

findDirectory :: BuildMaps -> FilePath -> DirectoryBuildMap
findDirectory (BuildMaps _ childrenMap) path =
  M.findWithDefault mempty path childrenMap

make :: Makefile -> BuildMaps
make makefile = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, target)
      | target <- makefileTargets makefile
      , outputPath <- targetOutputs target
      ]
    childrenMap =
      M.fromListWith mappend $

      [ (takeDirectory outputPath, mempty { dbmTargets = [pairWithTargetRep target] })
      | (outputPath, target) <- outputs
      ] ++

      [ (outPatDir, mempty { dbmPatterns = [targetPattern] })
      | targetPattern <- makefilePatterns makefile
      , outPatDir <- nub (map Makefile.filePatternDirectory (targetOutputs targetPattern))
      ]

    pairWithTargetRep target = (computeTargetRep target, target)

    buildMap =
      M.fromListWithKey (\path -> error $ "Overlapping output paths for: " ++ show path)
      [ (outputPath, pairWithTargetRep target)
      | (outputPath, target) <- outputs ]
