{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Main (main) where

import qualified Prelude.Compat as Prelude
import           Prelude.Compat hiding (FilePath, show)

import qualified Buildsome
import           Buildsome (Buildsome)
import qualified Buildsome.Chart as Chart
import qualified Buildsome.ClangCommands as ClangCommands
import qualified Buildsome.Color as Color
import qualified Buildsome.CompatMakefile as CompatMakefile
import           Buildsome.Db (Db, Reason)
import qualified Buildsome.MemoParseMakefile as MemoParseMakefile
import           Buildsome.Opts (Opts(..), Opt(..))
import qualified Buildsome.Opts as Opts
import qualified Control.Exception as E
import           Control.Monad (unless, forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           Lib.ByteString (unprefixed)
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Directory (getMFileStatus)
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Printer (Printer)
import qualified Lib.Printer as Printer
import           Lib.ScanFileUpwards (scanFileUpwards)
import           Lib.Show (show)
import           Lib.TimeIt (timeIt)
import qualified Lib.Version as Version
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix
import           System.Posix.IO (stdOutput)
import           System.Posix.Terminal (queryTerminal)

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
  , targetsRequestExtraOutputs :: Opts.ExtraOutputs
  }

ntraverseTargetsRequest ::
  Applicative f =>
  ([FilePath] -> f [FilePath]) ->
  (Reason -> f Reason) ->
  (Opts.ExtraOutputs -> f Opts.ExtraOutputs) ->
  TargetsRequest -> f TargetsRequest
ntraverseTargetsRequest
  onTargetPaths onReason onExtraOutputs
  (TargetsRequest paths reason extraOutputs)
  = TargetsRequest <$> onTargetPaths paths <*> onReason reason <*> onExtraOutputs extraOutputs

data Requested = RequestedClean | RequestedTargets TargetsRequest

traverseRequested ::
  Applicative f => (TargetsRequest -> f TargetsRequest) -> Requested -> f Requested
traverseRequested _ RequestedClean = pure RequestedClean
traverseRequested f (RequestedTargets x) = RequestedTargets <$> f x

data BadCommandLine = BadCommandLine (ColorText -> ByteString) String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine render msg) =
    BS8.unpack $ render $ cError $ "Invalid command line options: " <> fromString msg
    where
      Color.Scheme{..} = Color.scheme

getRequestedTargets :: Printer -> Opts.ExtraOutputs -> [ByteString] -> IO Requested
getRequestedTargets _ _ ["clean"] = return RequestedClean
getRequestedTargets printer extraOutputs ts
  | "clean" `elem` ts =
    E.throwIO $
    BadCommandLine (Printer.render printer) "Clean must be requested exclusively"
  | otherwise =
    return $ RequestedTargets TargetsRequest
      { targetsRequestPaths = requestPaths
      , targetsRequestReason = reason
      , targetsRequestExtraOutputs = extraOutputs
      }
  where
    (requestPaths, reason) = case ts of
      [] -> (["default"], "implicit 'default' target" :: ColorText)
      _ -> (ts, "explicit request from cmdline")

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
  (Db -> Opt -> Requested -> FilePath -> Makefile -> IO ()) -> IO ()
handleOpts printer GetVersion _ =
  Printer.rawPrintStrLn printer $ "buildsome " <> Version.version
handleOpts printer (Opts opt) body = do
  origMakefilePath <-
    case optMakefilePath opt of
    Nothing ->
      maybe (E.throwIO (MakefileScanFailed (Printer.render printer))) return =<<
      scanFileUpwards standardMakeFilename
    Just path -> specifiedMakefile printer path
  (origCwd, finalMakefilePath) <- switchDirectory origMakefilePath
  let inOrigCwd =
        FilePath.canonicalizePathAsRelative . (origCwd </>)
      targetInOrigCwd =
        FilePath.canonicalizePathAsRelative .
        case optMakefilePath opt of
        -- If we found the makefile by scanning upwards, prepend
        -- original cwd to avoid losing it:
        Nothing -> (origCwd </>)
        -- Otherwise: there's no useful original cwd:
        Just _ -> id
  rawRequested <-
    getRequestedTargets printer (optExtraOutputs opt) (optRequestedTargets opt)
  requested <-
    traverseRequested
    (ntraverseTargetsRequest
     (mapM targetInOrigCwd) -- <- on target paths
     pure                   -- <- on reason
     (Opts.extraOutputsAtFilePaths inOrigCwd) -- <- onExtraOutputs
    ) rawRequested
  Buildsome.withDb finalMakefilePath $ \db -> do
    makefile <-
      parseMakefile printer db origMakefilePath finalMakefilePath (optVars opt)
    let flags = flagsOfVars (Makefile.makefileWeakVars makefile)
    if optHelpFlags opt
      then showHelpFlags flags
      else do
        verifyValidFlags flags (optWiths opt ++ optWithouts opt)
        body db opt requested finalMakefilePath makefile

handleRequested :: Buildsome -> Printer -> Requested -> IO ()
handleRequested buildsome printer RequestedClean = Buildsome.clean printer buildsome
handleRequested
  buildsome printer
  (RequestedTargets
   (TargetsRequest requestedTargetPaths reason
    (Opts.ExtraOutputs mChartPath mClangCommandsPath compatMakefile)))
  = do
      Buildsome.BuiltTargets rootTargets slaveStats <-
        Buildsome.want printer buildsome reason requestedTargetPaths
      maybe (return ()) (Chart.make slaveStats) mChartPath
      cwd <- Posix.getWorkingDirectory
      maybe (return ()) (ClangCommands.make cwd slaveStats rootTargets) mClangCommandsPath
      case compatMakefile of
        Opts.NoCompatMakefile -> return ()
        Opts.CompatMakefile ->
          CompatMakefile.make (Buildsome.bsPhoniesSet buildsome) cwd slaveStats rootTargets "compat-makefile"

main :: IO ()
main = do
  setBuffering
  setNumCapabilities =<< getNumProcessors
  opts <- Opts.get
  render <- getColorRender opts
  printer <- Printer.new render $ Printer.Id 0
  handleOpts printer opts $
    \db opt requested finalMakefilePath makefile -> do
      Buildsome.with printer db finalMakefilePath makefile opt $ \buildsome ->
        handleRequested buildsome printer requested
