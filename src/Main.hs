{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Main (main) where

import Buildsome (Buildsome)
import Buildsome.Db (Db, Reason)
import Buildsome.Opts (Opts(..), Opt(..))
import Control.Monad
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.String (IsString(..))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import GHC.Conc (setNumCapabilities, getNumProcessors)
import Lib.ByteString (unprefixed)
import Lib.ColorText (ColorText)
import Lib.Directory (getMFileStatus)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (Makefile)
import Lib.Printer (Printer)
import Lib.ScanFileUpwards (scanFileUpwards)
import Lib.Show (show)
import Lib.Sigint (installSigintHandler)
import Lib.TimeIt (timeIt)
import Prelude hiding (FilePath, show)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import qualified Buildsome
import qualified Buildsome.Color as Color
import qualified Buildsome.MemoParseMakefile as MemoParseMakefile
import qualified Buildsome.Opts as Opts
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
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

data SpecifiedInexistentMakefilePath =
  SpecifiedInexistentMakefilePath (ColorText -> ByteString) FilePath
  deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath render path) =
    BS8.unpack $ render $ cError $ mconcat
    ["Specified makefile path: ", cPath (show path), " does not exist"]
    where
      Color.Scheme{..} = Color.scheme
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: Printer -> FilePath -> IO FilePath
specifiedMakefile printer path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath (Printer.render printer) path
    Just stat
      | Posix.isDirectory stat -> return $ path </> standardMakeFilename
      | otherwise -> return path

data TargetsRequest = TargetsRequest
  { targetsRequestPaths :: [FilePath]
  , targetsRequestReason :: Reason
  , targetsRequestChartPath :: Maybe FilePath
  }

data Requested = RequestedClean | RequestedTargets TargetsRequest

data BadCommandLine = BadCommandLine (ColorText -> ByteString) String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine render msg) =
    BS8.unpack $ render $ cError $ "Invalid command line options: " <> fromString msg
    where
      Color.Scheme{..} = Color.scheme

getRequestedTargets :: Printer -> Maybe FilePath -> [ByteString] -> IO Requested
getRequestedTargets printer (Just _) ["clean"] =
  E.throwIO $ BadCommandLine (Printer.render printer) "Clean requested with charts"
getRequestedTargets _ Nothing ["clean"] = return RequestedClean
getRequestedTargets _ mChartPath [] = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ["default"]
  , targetsRequestReason = "implicit 'default' target"
  , targetsRequestChartPath = mChartPath }
getRequestedTargets printer mChartPath ts
  | "clean" `elem` ts =
    E.throwIO $
    BadCommandLine (Printer.render printer) "Clean must be requested exclusively"
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

parseMakefile :: Printer -> Db -> FilePath -> FilePath -> Makefile.Vars -> IO Makefile
parseMakefile printer db origMakefilePath finalMakefilePath vars = do
  cwd <- Posix.getWorkingDirectory
  let absFinalMakefilePath = cwd </> finalMakefilePath
  (parseTime, (isHit, makefile)) <- timeIt $ do
    (isHit, rawMakefile) <- MemoParseMakefile.memoParse db absFinalMakefilePath vars
    makefile <- Makefile.onMakefilePaths FilePath.canonicalizePathAsRelative rawMakefile
    Makefile.verifyPhonies makefile
    return (isHit, makefile)
  let msg =
        case isHit of
        MemoParseMakefile.Hit -> "Got makefile from cache: "
        MemoParseMakefile.Miss -> "Parsed makefile: "
  Printer.rawPrintStrLn printer $ mconcat
    [ msg, cPath (show origMakefilePath)
    , " (took ", cTiming (show parseTime <> "sec"), ")"]
  return makefile
  where
    Color.Scheme{..} = Color.scheme

type InOrigCwd = [FilePath] -> IO [FilePath]

data MakefileScanFailed = MakefileScanFailed (ColorText -> ByteString) deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show (MakefileScanFailed render) =
    BS8.unpack $ render $ cError $ mconcat
    [ "ERROR: Cannot find a file named "
    , cPath (show standardMakeFilename)
    , " in this directory or any of its parents"
    ]
    where
      Color.Scheme{..} = Color.scheme

-- TODO: Extract the --enable/disable colors outside of the
-- version/etc separation.
getColorRender :: Opts -> IO (ColorText -> ByteString)
getColorRender GetVersion = return ColorText.stripColors
getColorRender (Opts opt) =
  case optColor opt of
  Opts.ColorDisable -> return ColorText.stripColors
  Opts.ColorEnable -> return ColorText.render
  Opts.ColorDefault -> do
    isTty <- queryTerminal stdOutput
    return $
      if isTty
      then ColorText.render
      else ColorText.stripColors

flagPrefix :: ByteString
flagPrefix = "FLAG_"

optVars :: Opt -> Makefile.Vars
optVars opt = M.fromList $ withs ++ withouts
  where
    asVars val = map $ \name -> (flagPrefix <> name, val)
    withs = asVars "enable" $ optWiths opt
    withouts = asVars "disable" $ optWithouts opt

flagsOfVars :: Makefile.Vars -> Makefile.Vars
flagsOfVars = M.fromList . mapMaybe filterFlag . M.toList
  where
    filterFlag (key, value) =
      fmap (flip (,) value) $ unprefixed flagPrefix key

ljust :: Int -> ByteString -> ByteString
ljust padding bs = bs <> BS8.replicate (padding - l) ' '
  where
    l = BS8.length bs

showHelpFlags :: Makefile.Vars -> IO ()
showHelpFlags flags = do
  BS8.putStrLn "Available flags:"
  -- TODO: Colors (depends on --enable/disbale color being outside)
  let varNameWidth = 1 + (foldl' max 0 . map BS8.length . M.keys) flags
  forM_ (M.toList flags) $ \(varName, defaultVal) ->
    BS8.putStrLn $ mconcat
      ["  ", ljust varNameWidth varName, "(default = ", show defaultVal, ")"]

verifyValidFlags :: Makefile.Vars -> [ByteString] -> IO ()
verifyValidFlags validFlags userFlags
  | null invalidUserFlags = return ()
  | otherwise = fail $ "Given non-existent flags: " ++ show invalidUserFlags
  where
    invalidUserFlags = filter (`M.notMember` validFlags) userFlags

handleOpts ::
  Printer -> Opts ->
  (Db -> Opt -> InOrigCwd -> Requested -> FilePath -> Makefile -> IO ()) -> IO ()
handleOpts printer GetVersion _ =
  Printer.rawPrintStrLn printer $ "buildsome " <> Version.version
handleOpts printer (Opts opt) body = do
  origMakefilePath <-
    case optMakefilePath opt of
    Nothing ->
      maybe (E.throwIO (MakefileScanFailed (Printer.render printer))) return =<<
      scanFileUpwards standardMakeFilename
    Just path -> specifiedMakefile printer path
  mChartsPath <- traverse FilePath.canonicalizePath $ optChartsPath opt
  (origCwd, finalMakefilePath) <- switchDirectory origMakefilePath
  let inOrigCwd =
        case optMakefilePath opt of
        -- If we found the makefile by scanning upwards, prepend
        -- original cwd to avoid losing it:
        Nothing -> mapM (FilePath.canonicalizePathAsRelative . (origCwd </>))
        -- Otherwise: there's no useful original cwd:
        Just _ -> return
  requested <- getRequestedTargets printer mChartsPath $ optRequestedTargets opt
  Buildsome.withDb finalMakefilePath $ \db -> do
    makefile <-
      parseMakefile printer db origMakefilePath finalMakefilePath (optVars opt)
    let flags = flagsOfVars (Makefile.makefileWeakVars makefile)
    if optHelpFlags opt
      then showHelpFlags flags
      else do
        verifyValidFlags flags (optWiths opt ++ optWithouts opt)
        body db opt inOrigCwd requested finalMakefilePath makefile

makeChart :: Slave.Stats -> FilePath -> IO ()
makeChart slaveStats filePath = do
  putStrLn $ "Writing chart to " ++ show filePath
  Chart.make slaveStats filePath

handleRequested :: Buildsome -> Printer -> InOrigCwd -> Requested -> IO ()
handleRequested buildsome printer inOrigCwd (RequestedTargets (TargetsRequest requestedTargets reason mChartPath)) = do
  requestedTargetPaths <- inOrigCwd requestedTargets
  slaveStats <- Buildsome.want printer buildsome reason requestedTargetPaths
  maybe (return ()) (makeChart slaveStats) mChartPath

handleRequested buildsome printer _ RequestedClean = Buildsome.clean printer buildsome

main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  opts <- Opts.get
  render <- getColorRender opts
  printer <- Printer.new render 0
  handleOpts printer opts $
    \db opt inOrigCwd requested finalMakefilePath makefile -> do
      installSigintHandler
      setBuffering
      Buildsome.with printer db finalMakefilePath makefile opt $ \buildsome ->
        handleRequested buildsome printer inOrigCwd requested
