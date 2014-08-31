{-# LANGUAGE OverloadedStrings #-}
module Buildsome.CompatMakefile
  ( make
  ) where

import Buildsome.BuildMaps (TargetRep)
import Buildsome.Stats (Stats)
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target)
import Lib.Parsec (showPos)
import Prelude hiding (FilePath)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Lib.Revisit as Revisit

type M = Revisit.M TargetRep Identity

onOneTarget :: FilePath -> Stats -> Target -> M [ByteString]
onOneTarget cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid (BuildMaps.computeTargetRep target) $ do
    depsLines <- depBuildCommands
    return $ myLines ++ depsLines
  where
    Target outputs _ _ cmds pos = target
    myLines =
      [ "#" <> BS8.pack (showPos pos)
      , BS8.unwords outputs <> ":" <>
        if null dependencies then "" else " " <> BS8.unwords (concatMap targetOutputs dependencies)
      ] ++
      map ("\t" <>) (BS8.lines cmds) ++
      [ ""
      ]
    dependencies =
      case Map.lookup (BuildMaps.computeTargetRep target) (Stats.ofTarget stats) of
      Nothing -> error "BUG: Stats does not contain targets that appear as root/dependencies"
      Just (_, _, deps) -> deps
    depBuildCommands = onMultipleTargets cwd stats dependencies

onMultipleTargets :: FilePath -> Stats -> [Target] -> M [ByteString]
onMultipleTargets cwd stats = fmap concat . mapM (onOneTarget cwd stats)

make :: FilePath -> Stats -> [Target] -> FilePath -> IO ()
make cwd stats rootTargets filePath = do
  putStrLn $ "Writing compat makefile to: " ++ show (cwd </> filePath)
  BS8.writeFile (BS8.unpack filePath) $ BS8.unlines $
    [ "# Auto-generated compatibility mode Makefile"
    , "# THIS MAKEFILE IS INVALID AS SOON AS ANY CHANGE OCCURS ANYWHERE. USE CAREFULLY."
    , "# make -f compat-makefile"
    ] ++
    (runIdentity . Revisit.run) (onMultipleTargets cwd stats rootTargets)
