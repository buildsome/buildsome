{-# LANGUAGE OverloadedStrings #-}
module Buildsome.ClangCommands
  ( make
  ) where

import Buildsome.BuildMaps (TargetRep)
import Buildsome.Stats (Stats)
import Data.Aeson ((.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target)
import Lib.Revisit (M)
import Prelude hiding (FilePath)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BS8L
import qualified Data.Map as Map
import qualified Lib.Revisit as Revisit

buildCommands :: FilePath -> Stats -> Target -> M TargetRep [Aeson.Value]
buildCommands cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid (BuildMaps.computeTargetRep target) $ do
    deps <- depBuildCommands
    return $ myBuildCommands ++ deps
  where
    myBuildCommands =
      case targetInputs target of
        [file]
          | not (BS8.null (targetCmds target)) ->
            [ Aeson.object
              [ "directory" .= BS8.unpack cwd
              , "command" .= BS8.unpack (targetCmds target)
              , "file" .= BS8.unpack file
              ]
            ]
        _ -> []
    depBuildCommands =
      case Map.lookup (BuildMaps.computeTargetRep target) (Stats.ofTarget stats) of
      Nothing ->
        error "BUG: Stats does not contain targets that appear as root/dependencies"
      Just (_, _, deps) -> buildCommandsTargets cwd stats deps

buildCommandsTargets :: FilePath -> Stats -> [Target] -> M TargetRep [Aeson.Value]
buildCommandsTargets cwd stats = fmap concat . mapM (buildCommands cwd stats)

make :: FilePath -> Stats -> [Target] -> FilePath -> IO ()
make cwd stats rootTargets filePath = do
  putStrLn $ "Writing clang commands to: " ++ show (cwd </> filePath)
  BS8L.writeFile (BS8.unpack filePath) $
    encodePretty $ reverse $
    Revisit.run (buildCommandsTargets cwd stats rootTargets)
