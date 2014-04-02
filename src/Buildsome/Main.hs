{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main (main) where

import Buildsome (Buildsome)
import Buildsome.Db (Reason)
import Buildsome.Opts (Opts(..), Opt(..))
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String (IsString(..))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lib.Directory (getMFileStatus)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (Makefile)
import Lib.Printer (Printer)
import Lib.ScanFileUpwards (scanFileUpwards)
import Lib.Show (show)
import Lib.Sigint (installSigintHandler)
import Lib.TimeIt (timeIt)
import Prelude hiding (FilePath, show)
import qualified Buildsome as Buildsome
import qualified Buildsome.Color as Color
import qualified Buildsome.Opts as Opts
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.Chart as Chart
import qualified Lib.ColorText as ColorText
import qualified Lib.FilePath as FilePath
import qualified Lib.Makefile as Makefile
import qualified Lib.Printer as Printer
import qualified Lib.Slave as Slave
import qualified Lib.Version as Version
import qualified Prelude
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

standardMakeFilename :: FilePath
standardMakeFilename = "Makefile"

data SpecifiedInexistentMakefilePath = SpecifiedInexistentMakefilePath FilePath deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath path) =
    ColorText.renderStr $ Color.error $ mconcat
    ["Specified makefile path: ", Color.path (show path), " does not exist"]
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: FilePath -> IO FilePath
specifiedMakefile path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath path
    Just stat
      | Posix.isDirectory stat -> return $ path </> "Makefile"
      | otherwise -> return path

data TargetsRequest = TargetsRequest
  { targetsRequestPaths :: [FilePath]
  , targetsRequestReason :: Reason
  , targetsRequestChartPath :: Maybe FilePath
  }

data Requested = RequestedClean | RequestedTargets TargetsRequest

data BadCommandLine = BadCommandLine String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine msg) =
    ColorText.renderStr $ Color.error $ "Invalid command line options: " <> fromString msg

getRequestedTargets :: Maybe FilePath -> [ByteString] -> IO Requested
getRequestedTargets (Just _) ["clean"] = E.throwIO $ BadCommandLine "Clean requested with charts"
getRequestedTargets Nothing ["clean"] = return RequestedClean
getRequestedTargets mChartPath [] = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ["default"]
  , targetsRequestReason = "implicit 'default' target"
  , targetsRequestChartPath = mChartPath }
getRequestedTargets mChartPath ts
  | "clean" `elem` ts = E.throwIO $ BadCommandLine "Clean must be requested exclusively"
  | otherwise = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ts
  , targetsRequestReason = "explicit request from cmdline"
  , targetsRequestChartPath = mChartPath }

setBuffering :: IO ()
setBuffering = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

switchDirectory :: FilePath -> IO (FilePath, FilePath)
switchDirectory makefilePath = do
  origCwd <- Posix.getWorkingDirectory
  unless (BS8.null cwd) $ do
    Posix.changeWorkingDirectory cwd
    fullCwd <- FilePath.canonicalizePath $ origCwd </> cwd
    BS8.putStrLn $ "make: Entering directory `" <> fullCwd <> "'"
  return (origCwd, file)
  where
    (cwd, file) = FilePath.splitFileName makefilePath

parseMakefile :: FilePath -> FilePath -> IO Makefile
parseMakefile origMakefilePath finalMakefilePath = do
  (parseTime, makefile) <- timeIt $ Makefile.onMakefilePaths FilePath.canonicalizePathAsRelative =<< Makefile.parse finalMakefilePath
  ColorText.putStrLn $ mconcat
    [ "Parsed makefile: ", Color.path (show origMakefilePath)
    , " (took ", Color.timing (show parseTime <> "sec"), ")"]
  return makefile

type InOrigCwd = [FilePath] -> IO [FilePath]

data MakefileScanFailed = MakefileScanFailed deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show MakefileScanFailed =
    ColorText.renderStr $ Color.error $ mconcat
    [ "ERROR: Cannot find a file named "
    , Color.path (show standardMakeFilename)
    , " in this directory or any of its parents"
    ]

handleOpts :: Opts -> IO (Maybe (Opt, InOrigCwd, Requested, FilePath, Makefile))
handleOpts GetVersion = do
  ver <- Version.get
  BS8.putStrLn $ "buildsome " <> ver
  return Nothing
handleOpts (Opts opt) = do
  origMakefilePath <-
    case optMakefilePath opt of
    Nothing ->
      maybe (E.throwIO MakefileScanFailed) return =<<
      scanFileUpwards standardMakeFilename
    Just path -> specifiedMakefile path
  mChartsPath <- traverse FilePath.canonicalizePath $ optChartsPath opt
  (origCwd, finalMakefilePath) <- switchDirectory origMakefilePath
  let inOrigCwd =
        case optMakefilePath opt of
        -- If we found the makefile by scanning upwards, prepend
        -- original cwd to avoid losing it:
        Nothing -> mapM (FilePath.canonicalizePathAsRelative . (origCwd </>))
        -- Otherwise: there's no useful original cwd:
        Just _ -> return
  requested <- getRequestedTargets mChartsPath $ optRequestedTargets opt
  makefile <- parseMakefile origMakefilePath finalMakefilePath
  return $ Just (opt, inOrigCwd, requested, finalMakefilePath, makefile)

makeChart :: Slave.Stats -> FilePath -> IO ()
makeChart slaveStats filePath = do
  putStrLn $ "Writing chart to " ++ show filePath
  Chart.make slaveStats filePath

handleRequested :: Buildsome -> Printer -> InOrigCwd -> Requested -> IO ()
handleRequested buildsome printer inOrigCwd (RequestedTargets (TargetsRequest requestedTargets reason mChartPath)) = do
  requestedTargetPaths <- inOrigCwd requestedTargets
  slaveStats <- Buildsome.want printer buildsome reason requestedTargetPaths
  maybe (return ()) (makeChart slaveStats) mChartPath

handleRequested buildsome _ _ RequestedClean = Buildsome.clean buildsome

main :: IO ()
main = do
  opts <- Opts.get
  mRes <- handleOpts opts
  case mRes of
    Nothing -> return ()
    Just (opt, inOrigCwd, requested, finalMakefilePath, makefile) -> do
      installSigintHandler
      setBuffering
      printer <- Printer.new 0
      Buildsome.with finalMakefilePath makefile opt $ \buildsome ->
        handleRequested buildsome printer inOrigCwd requested
