module Buildsome.ClangCommands
  ( make
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.BuildMaps (TargetRep)
import Buildsome.Stats (Stats)
import Data.Aeson ((.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target, targetInterpolatedCmds)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BS8L
import qualified Data.Map as Map
import qualified Lib.Revisit as Revisit

type M = Revisit.M TargetRep Identity

buildCommands :: FilePath -> Stats -> Target -> M [Aeson.Value]
buildCommands cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid (BuildMaps.computeTargetRep target) $ do
    deps <- depBuildCommands
    pure $ myBuildCommands ++ deps
  where
    myBuildCommands =
      case targetInputs target of
        [file]
          | not (BS8.null (targetInterpolatedCmds target)) ->
            [ Aeson.object
              [ "directory" .= BS8.unpack cwd
              , "command" .= BS8.unpack (targetInterpolatedCmds target)
              , "file" .= BS8.unpack file
              ]
            ]
        _ -> []
    depBuildCommands =
      case Map.lookup (BuildMaps.computeTargetRep target) (Stats.ofTarget stats) of
      Nothing -> pure [] -- ok because some dependencies have no rule?
      Just targetStats -> buildCommandsTargets cwd stats $ Stats.tsDirectDeps targetStats

buildCommandsTargets :: FilePath -> Stats -> [Target] -> M [Aeson.Value]
buildCommandsTargets cwd stats = fmap concat . traverse (buildCommands cwd stats)

make :: FilePath -> Stats -> [Target] -> FilePath -> IO ()
make cwd stats rootTargets filePath = do
  putStrLn $ "Writing clang commands to: " ++ show (cwd </> filePath)
  BS8L.writeFile (BS8.unpack filePath) $
    encodePretty $ reverse $
    runIdentity $ Revisit.run (buildCommandsTargets cwd stats rootTargets)
