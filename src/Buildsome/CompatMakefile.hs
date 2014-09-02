{-# LANGUAGE OverloadedStrings #-}
module Buildsome.CompatMakefile
  ( Phonies, make
  ) where

import Buildsome.BuildMaps (TargetRep)
import Buildsome.Stats (Stats)
import Control.Monad (filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target)
import Lib.Parsec (showPos)
import Prelude hiding (FilePath)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lib.Revisit as Revisit
import qualified System.Directory as Directory

type M = Revisit.M TargetRep IO

data MakefileTarget = MakefileTarget
  { makefileTargetPath :: FilePath
  , makefileTargetDirs :: [FilePath]
  , isDirectory :: Bool
  }

makefileTarget :: Target -> IO MakefileTarget
makefileTarget target = do
  repIsDir <- isDir repPath
  targetDirs <- filterM isDir (targetOutputs target)
  return MakefileTarget
    { makefileTargetPath = if repIsDir then repPath </> ".dir" else repPath
    , makefileTargetDirs = targetDirs
    , isDirectory = repIsDir
    }
  where
    isDir = Directory.doesDirectoryExist . BS8.unpack
    repPath = BuildMaps.targetRepPath $ BuildMaps.computeTargetRep target

targetCmdLines :: MakefileTarget -> Target -> [ByteString]
targetCmdLines tgt target =
  ["rm -rf " <> dir | dir <- makefileTargetDirs tgt] ++
  (BS8.lines . targetCmds) target ++
  ["touch " <> makefileTargetPath tgt | isDirectory tgt]

type Phonies = Set FilePath

onOneTarget :: Phonies -> FilePath -> Stats -> Target -> M [ByteString]
onOneTarget phoniesSet cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid targetRep $ do
    depsLines <- depBuildCommands
    tgt <- lift $ makefileTarget target
    depTgts <- mapM (lift . makefileTarget) dependencies
    let
      myLines =
        [ "#" <> BS8.pack (showPos (targetPos target))
        ] ++
        [ ".PHONY: " <> makefileTargetPath tgt
        | makefileTargetPath tgt `Set.member` phoniesSet
        ] ++
        [ makefileTargetPath tgt <> ":" <>
          if null dependencies then "" else " " <> BS8.unwords (map makefileTargetPath depTgts)
        ] ++
        map ("\t" <>) (targetCmdLines tgt target) ++
        [ ""
        ]
    return $ myLines ++ depsLines
  where
    targetRep = BuildMaps.computeTargetRep target
    dependencies =
      case Map.lookup targetRep (Stats.ofTarget stats) of
      Nothing -> error "BUG: Stats does not contain targets that appear as root/dependencies"
      Just targetStats -> Stats.tsDirectDeps targetStats
    depBuildCommands = onMultipleTargets phoniesSet cwd stats dependencies

onMultipleTargets :: Phonies -> FilePath -> Stats -> [Target] -> M [ByteString]
onMultipleTargets phoniesSet cwd stats = fmap concat . mapM (onOneTarget phoniesSet cwd stats)

make :: Phonies -> FilePath -> Stats -> [Target] -> FilePath -> IO ()
make phoniesSet cwd stats rootTargets filePath = do
  putStrLn $ "Writing compat makefile to: " ++ show (cwd </> filePath)
  makefileLines <- Revisit.run (onMultipleTargets phoniesSet cwd stats rootTargets)
  BS8.writeFile (BS8.unpack filePath) $
    BS8.unlines $
    [ "# Auto-generated compatibility mode Makefile"
    , "# THIS MAKEFILE IS INVALID AS SOON AS ANY CHANGE OCCURS ANYWHERE. USE CAREFULLY."
    , "# make -f compat-makefile"
    ] ++ makefileLines
