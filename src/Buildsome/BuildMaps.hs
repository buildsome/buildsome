module Buildsome.BuildMaps
  ( TargetRep(..), computeTargetRep
  , TargetDesc(..), descOfTarget
  , DirectoryBuildMap(..)
  , BuildMaps(..)
  , make
  , TargetKind(..)
  , find
  , findDirectory
  ) where

import qualified Buildsome.Print as Print
import           Control.Monad
import qualified Data.ByteString.Char8 as BS8
import           Data.List (nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Lib.FilePath (FilePath, takeDirectory)
import           Lib.Makefile (Makefile(..), TargetType(..), Target, Pattern)
import qualified Lib.Makefile as Makefile
import qualified Lib.StringPattern as StringPattern

import           Prelude.Compat hiding (FilePath)

-- | Unique identifier of the target.
newtype TargetRep = TargetRep { targetRepPath :: FilePath } -- We use the minimum output path as the
                                                            -- target key/representative. It's ok to
                                                            -- do this because each target outputs
                                                            -- can't overlap
  deriving (Eq, Ord, Show)
computeTargetRep :: Target -> TargetRep
computeTargetRep = TargetRep . minimum . targetOutputs

data TargetDesc = TargetDesc
    { tdRep :: TargetRep
    , tdTarget :: Target
    } deriving (Show)

descOfTarget :: Target -> TargetDesc
descOfTarget target = TargetDesc (computeTargetRep target) target

data DirectoryBuildMap = DirectoryBuildMap
    { dbmTargets :: [TargetDesc]
    , dbmPatterns :: [Pattern]
    } deriving (Show)
instance Semigroup DirectoryBuildMap where
    DirectoryBuildMap x0 x1 <> DirectoryBuildMap y0 y1 =
        DirectoryBuildMap (x0 <> y0) (x1 <> y1)
instance Monoid DirectoryBuildMap where
    mempty = DirectoryBuildMap mempty mempty

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath TargetDesc -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath DirectoryBuildMap
  }

data TargetKind = TargetPattern | TargetSimple
  deriving (Eq)

find :: BuildMaps -> FilePath -> Maybe (TargetKind, TargetDesc)
find (BuildMaps buildMap childrenMap) path =
  -- Allow specific/simple matches to override pattern matches
  ((,) TargetSimple <$> simpleMatch) `mplus`
  ((,) TargetPattern <$> patternMatch)
  where
    simpleMatch = path `M.lookup` buildMap
    patterns = dbmPatterns $ M.findWithDefault mempty (takeDirectory path) childrenMap
    instantiate pattern = (,) pattern <$> Makefile.instantiatePatternByOutput path pattern
    patternMatch =
      case mapMaybe instantiate patterns of
      [] -> Nothing
      [(_, target)] -> Just $ descOfTarget target
      targets ->
        error $ BS8.unpack $ mconcat
        [ "Multiple matching patterns for: ", BS8.pack (show path), "\n"
        , BS8.unlines $
          map (showPattern . fst) targets
        ]
    showPattern pattern =
      Print.posText (targetPos pattern) <> showPatternOutputs pattern
    showPatternOutputs pattern =
      BS8.unwords $
      map (StringPattern.toString . Makefile.filePatternFile) $
      targetOutputs pattern

findDirectory :: BuildMaps -> FilePath -> DirectoryBuildMap
findDirectory (BuildMaps _ childrenMap) path =
  M.findWithDefault mempty path childrenMap

make :: Makefile -> BuildMaps
make makefile = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, descOfTarget target)
      | target <- makefileTargets makefile
      , outputPath <- targetOutputs target
      ]
    childrenMap =
      M.fromListWith mappend $
      [ (takeDirectory outputPath, mempty { dbmTargets = [targetDesc] })
      | (outputPath, targetDesc) <- outputs
      ] ++

      [ (outPatDir, mempty { dbmPatterns = [targetPattern] })
      | targetPattern <- makefilePatterns makefile
      , outPatDir <- nub (map Makefile.filePatternDirectory (targetOutputs targetPattern))
      ]

    overlappingOutputs path (TargetDesc _ a) (TargetDesc _ b) =
      error $ "Overlapping output paths for: " ++ show path ++ " at:\n" ++
      show (targetPos a) ++ "vs.\n" ++ show (targetPos b)
    buildMap =
      M.fromListWithKey overlappingOutputs
      [ (outputPath, targetDesc)
      | (outputPath, targetDesc) <- outputs ]
