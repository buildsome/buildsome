module Buildsome.CompatMakefile
  ( Phonies, make
  ) where

import           Buildsome.BuildMaps (TargetRep)
import qualified Buildsome.BuildMaps as BuildMaps
import           Buildsome.Stats (Stats)
import qualified Buildsome.Stats as Stats
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import           Data.List (partition)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Lib.Directory as Directory
import           Lib.FilePath (FilePath, (</>))
import           Lib.List (partitionA)
import           Lib.Makefile (TargetType(..), Target, targetInterpolatedCmds)
import           Lib.Parsec (showPos)
import qualified Lib.Revisit as Revisit
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath)

isDir :: FilePath -> IO Bool
isDir path = maybe False Posix.isDirectory <$> Directory.getMFileStatus path

type M = Revisit.M TargetRep IO

data MakefileTarget = MakefileTarget
  { makefileTargetPaths :: [FilePath]
  , makefileTargetDirs :: [FilePath]
  , isDirectory :: Bool
  }

makefileTarget :: Target -> IO MakefileTarget
makefileTarget target = do
  repIsDir <- isDir repPath
  (targetOutputDirs, targetOutputFiles) <- partitionA isDir (targetOutputs target)
  return MakefileTarget
    { makefileTargetPaths = map (</> ".dir") targetOutputDirs ++ targetOutputFiles
    , makefileTargetDirs = targetOutputDirs
    , isDirectory = repIsDir
    }
  where
    repPath = BuildMaps.targetRepPath $ BuildMaps.computeTargetRep target

-- TODO use Builder API for more efficient escaping, see
-- https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Builder-Prim.html
escape :: ByteString -> ByteString
escape xs = "'" <> BS8.concatMap f xs <> "'"
  where
    f '\0' = error $ "Unsupported character NUL in '" ++ show xs ++ "'"
    -- In bash, to escape ' while in a '-quoted string, we must close
    -- the ' then write an escaped \' and then re-open the '
    f '\'' = "'\\''"
    f x    = BS8.singleton x


prependAll :: Monoid a => a -> [a] -> [a]
prependAll prefix = map (prefix <>)

trimLeadingSpaces :: ByteString -> ByteString
trimLeadingSpaces = BS8.dropWhile Char.isSpace

targetCmdLines :: MakefileTarget -> Target -> [ByteString]
targetCmdLines tgt target = concat
  [ prependAll "rm -rf " $ makefileTargetDirs tgt
  , map trimLeadingSpaces . BS8.lines $ targetInterpolatedCmds target
  , [ "touch " <> path | isDirectory tgt, path <- makefileTargetPaths tgt ]
  ]

type Phonies = Set FilePath

sortNub :: [ByteString] -> [ByteString]
sortNub = Set.toList . Set.fromList

onOneTarget :: Phonies -> FilePath -> Stats -> Target -> M [ByteString]
onOneTarget phoniesSet cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid targetRep $ do
    depsLines <- depBuildCommands
    tgt <- lift $ makefileTarget target
    directDepsPaths <-
        lift
        $ concatMap makefileTargetPaths <$> mapM makefileTarget
        (Stats.tsDirectDeps targetStats)
    let
      (phonies, nonPhonies) = partition (`Set.member` phoniesSet) $ makefileTargetPaths tgt
      targetDecl =
        [ "T := " <> BS8.unwords (makefileTargetPaths tgt)
        , "D := " <> BS8.unwords (sortNub $ directDepsPaths <> inputs)
        , "$(T): $(D)"
        ]
      myLines = concat
        [ [ "#" <> (BS8.pack . showPos . targetPos) target ]
        , prependAll ".PHONY: " phonies
        , targetDecl
        , prependAll "\t" $ map (("rm -f " <>) . escape) nonPhonies
        , prependAll "\t" $ targetCmdLines tgt target
        , [ "" ]
        ]
    return $ myLines ++ depsLines
  where
    inputs =
      fromMaybe
      (error "compat makefile requested without tsExistingInputs being calculated?!")
      (Stats.tsExistingInputs targetStats)
    targetRep = BuildMaps.computeTargetRep target
    directDeps = Stats.tsDirectDeps targetStats
    targetStats =
      fromMaybe
      (error $ mconcat
       [ "BUG: Stats does not contain targets that appear as root/dependencies: "
       , show $ BuildMaps.targetRepPath targetRep ])
      $ Map.lookup targetRep (Stats.ofTarget stats)
    depBuildCommands = onMultipleTargets phoniesSet cwd stats directDeps

onMultipleTargets :: Phonies -> FilePath -> Stats -> [Target] -> M [ByteString]
onMultipleTargets phoniesSet cwd stats = fmap concat . mapM (onOneTarget phoniesSet cwd stats)

make :: Phonies -> FilePath -> Stats -> [Target] -> FilePath -> IO ()
make phoniesSet cwd stats rootTargets filePath = do
  putStrLn $ "Writing compat makefile to: " ++ show (cwd </> filePath)
  makefileLines <- Revisit.run (onMultipleTargets phoniesSet cwd stats rootTargets)
  BS8.writeFile (BS8.unpack filePath) $
    BS8.unlines $
    [ "# Auto-generated compatibility mode Makefile"
    , "# THIS MAKEFILE IS INVALID AS SOON AS ANY CHANGE OCCURS ANYWHERE"
    , "# ON THE FILE SYSTEM (even outside your project). USE CAREFULLY."
    , "# make -f compat-makefile"
    ] ++ makefileLines

