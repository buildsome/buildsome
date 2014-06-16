{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
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
import qualified Buildsome
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
standardMakeFilename = "Buildsome.mk"

data SpecifiedInexistentMakefilePath = SpecifiedInexistentMakefilePath Color.Scheme FilePath deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath Color.Scheme{..} path) =
    ColorText.renderStr $ cError $ mconcat
    ["Specified makefile path: ", cPath (show path), " does not exist"]
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: Color.Scheme -> FilePath -> IO FilePath
specifiedMakefile colors path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath colors path
    Just stat
      | Posix.isDirectory stat -> return $ path </> standardMakeFilename
      | otherwise -> return path

data TargetsRequest = TargetsRequest
  { targetsRequestPaths :: [FilePath]
  , targetsRequestReason :: Reason
  , targetsRequestChartPath :: Maybe FilePath
  }

data Requested = RequestedClean | RequestedTargets TargetsRequest

data BadCommandLine = BadCommandLine Color.Scheme String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine Color.Scheme{..} msg) =
    ColorText.renderStr $ cError $ "Invalid command line options: " <> fromString msg

getRequestedTargets :: Color.Scheme -> Maybe FilePath -> [ByteString] -> IO Requested
getRequestedTargets colors (Just _) ["clean"] = E.throwIO $ BadCommandLine colors "Clean requested with charts"
getRequestedTargets _ Nothing ["clean"] = return RequestedClean
getRequestedTargets _ mChartPath [] = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ["default"]
  , targetsRequestReason = "implicit 'default' target"
  , targetsRequestChartPath = mChartPath }
getRequestedTargets colors mChartPath ts
  | "clean" `elem` ts = E.throwIO $ BadCommandLine colors "Clean must be requested exclusively"
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

parseMakefile :: Color.Scheme -> FilePath -> FilePath -> IO Makefile
parseMakefile Color.Scheme{..} origMakefilePath finalMakefilePath = do
  (parseTime, makefile) <- timeIt $ Makefile.onMakefilePaths FilePath.canonicalizePathAsRelative =<< Makefile.parse finalMakefilePath
  ColorText.putStrLn $ mconcat
    [ "Parsed makefile: ", cPath (show origMakefilePath)
    , " (took ", cTiming (show parseTime <> "sec"), ")"]
  return makefile

type InOrigCwd = [FilePath] -> IO [FilePath]

data MakefileScanFailed = MakefileScanFailed Color.Scheme deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show (MakefileScanFailed Color.Scheme{..}) =
    ColorText.renderStr $ cError $ mconcat
    [ "ERROR: Cannot find a file named "
    , cPath (show standardMakeFilename)
    , " in this directory or any of its parents"
    ]

getColors :: Opts -> IO Color.Scheme
getColors GetVersion = return Color.nonColorScheme
getColors (Opts opt) =
  case optColor opt of
  Opts.ColorDisable -> return Color.nonColorScheme
  Opts.ColorEnable -> return Color.defaultScheme
  Opts.ColorDefault -> Color.schemeForTerminal

handleOpts :: Color.Scheme -> Opts -> IO (Maybe (Opt, InOrigCwd, Requested, FilePath, Makefile))
handleOpts _ GetVersion = do
  BS8.putStrLn $ "buildsome " <> Version.version
  return Nothing
handleOpts colors (Opts opt) = do
  origMakefilePath <-
    case optMakefilePath opt of
    Nothing ->
      maybe (E.throwIO (MakefileScanFailed colors)) return =<<
      scanFileUpwards standardMakeFilename
    Just path -> specifiedMakefile colors path
  mChartsPath <- traverse FilePath.canonicalizePath $ optChartsPath opt
  (origCwd, finalMakefilePath) <- switchDirectory origMakefilePath
  let inOrigCwd =
        case optMakefilePath opt of
        -- If we found the makefile by scanning upwards, prepend
        -- original cwd to avoid losing it:
        Nothing -> mapM (FilePath.canonicalizePathAsRelative . (origCwd </>))
        -- Otherwise: there's no useful original cwd:
        Just _ -> return
  requested <- getRequestedTargets colors mChartsPath $ optRequestedTargets opt
  makefile <- parseMakefile colors origMakefilePath finalMakefilePath
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
  colors <- getColors opts
  mRes <- handleOpts colors opts
  case mRes of
    Nothing -> return ()
    Just (opt, inOrigCwd, requested, finalMakefilePath, makefile) -> do
      installSigintHandler
      setBuffering
      printer <- Printer.new 0
      Buildsome.with colors finalMakefilePath makefile opt $ \buildsome ->
        handleRequested buildsome printer inOrigCwd requested
