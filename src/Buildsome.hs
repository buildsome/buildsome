{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Buildsome
  ( Buildsome, with, withDb
  , clean
  , BuiltTargets(..)
  , want
  ) where

import Buildsome.Db (Db, IRef(..), Reason)
import Buildsome.FileContentDescCache (fileContentDescOfStat)
import Buildsome.Opts (Opt(..))
import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Exception.Async (handleSync)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lib.AnnotatedException (annotateException)
import Lib.BuildId (BuildId)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.ColorText (ColorText)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing)
import Lib.Exception (finally)
import Lib.FSHook (FSHook, OutputBehavior(..), OutputEffect(..), KeepsOldContent(..))
import Lib.FileDesc (fileModeDescOfStat, fileStatDescOfStat)
import Lib.FilePath (FilePath, (</>), (<.>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import Lib.Makefile (Makefile(..), TargetType(..), Target, targetAllInputs)
import Lib.Once (once)
import Lib.Parallelism (Parallelism)
import Lib.Parsec (showPos)
import Lib.Printer (Printer, printStrLn)
import Lib.Show (show)
import Lib.ShowBytes (showBytes)
import Lib.Slave (Slave)
import Lib.StdOutputs (StdOutputs(..))
import Lib.TimeIt (timeIt)
import Prelude hiding (FilePath, show)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..))
import Text.Parsec (SourcePos)
import qualified Buildsome.Clean as Clean
import qualified Buildsome.Color as Color
import qualified Buildsome.Db as Db
import qualified Buildsome.MagicFiles as MagicFiles
import qualified Buildsome.Meddling as Meddling
import qualified Buildsome.Opts as Opts
import qualified Buildsome.Print as Print
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Lib.BuildId as BuildId
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.ColorText as ColorText
import qualified Lib.Directory as Dir
import qualified Lib.FSHook as FSHook
import qualified Lib.FilePath as FilePath
import qualified Lib.Fresh as Fresh
import qualified Lib.Map as LibMap
import qualified Lib.Parallelism as Parallelism
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Slave as Slave
import qualified Lib.Timeout as Timeout
import qualified Prelude
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

type Parents = [(TargetRep, Reason)]

data Buildsome = Buildsome
  { -- static:
    bsOpts :: Opt
  , bsMakefile :: Makefile
  , bsPhoniesSet :: Set FilePath
  , bsBuildId :: BuildId
  , bsRootPath :: FilePath
  , bsBuildMaps :: BuildMaps
    -- dynamic:
  , bsDb :: Db
  , bsFsHook :: FSHook
  , bsSlaveByTargetRep :: IORef (Map TargetRep (MVar Slave))
  , bsParallelism :: Parallelism
  , bsFreshPrinterIds :: Fresh Printer.Id
  , bsFastKillBuild :: E.SomeException -> IO ()
  , bsRender :: ColorText -> ByteString
  }

cancelAllSlaves :: Printer -> Buildsome -> IO ()
cancelAllSlaves printer bs = go 0
  where
    Color.Scheme{..} = Color.scheme
    timeoutWarning time slave =
      Timeout.warning time $
      mconcat ["Slave ", Slave.str slave, " did not cancel in ", show time, "!"]
    go alreadyCancelled = do
      curSlaveMap <- readIORef $ bsSlaveByTargetRep bs
      slaves <-
        forM (M.toList curSlaveMap) $
        \(targetRep, mvar) ->
        Timeout.warning (Timeout.millis 100)
        (Printer.render printer
         ("Slave MVar for " <> cTarget (show targetRep) <> " not populated in 100 millis!")) $
        readMVar mvar
      let count = length slaves
      unless (alreadyCancelled >= count) $ do
        forM_ slaves $ \slave -> timeoutWarning (Timeout.millis 100) slave $
          Slave.cancel slave
        forM_ slaves $ \slave -> timeoutWarning (Timeout.seconds 2) slave $
          Slave.waitCatch slave
        -- Make sure to cancel any potential new slaves that were
        -- created during cancellation
        go count

verbosePutStrln :: Buildsome -> String -> IO ()
verbosePutStrln buildsome str =
  when verbose $ putStrLn str
  where
    verbose = Opts.verbosityGeneral $ optVerbosity $ bsOpts buildsome

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = FilePath.takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      gitIgnoreBasePath = dir </> gitignoreBaseName
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  verbosePutStrln buildsome "Updating .gitignore"
  base <- Dir.catchDoesNotExist
          (fmap BS8.lines $ BS8.readFile $  BS8.unpack gitIgnoreBasePath)
          $ return []
  let
    generatedPaths =
      map (("/" <>) . FilePath.makeRelative dir) $
        extraIgnored ++ S.toList (outputs <> leaked)
  BS8.writeFile (BS8.unpack gitIgnorePath) $ BS8.unlines $
    header ++ generatedPaths ++ base
  where
    gitignoreBaseName = ".gitignore-base"
    header = ["# AUTO GENERATED FILE - DO NOT EDIT",
              BS8.concat
             ["# If you need to ignore files not managed by buildsome, add to ", gitignoreBaseName]]

data BuildTargetEnv = BuildTargetEnv
  { bteBuildsome :: Buildsome
  , btePrinter :: Printer
  , bteReason :: Reason
  , bteParents :: Parents
  }

data BuiltTargets = BuiltTargets
  { builtTargets :: [Target]
  , builtStats :: Slave.Stats
  }
instance Monoid BuiltTargets where
  mempty = BuiltTargets mempty mempty
  mappend (BuiltTargets a1 b1) (BuiltTargets a2 b2) =
    BuiltTargets (mappend a1 a2) (mappend b1 b2)

mkSlavesForPaths :: BuildTargetEnv -> [FilePath] -> IO [Slave]
mkSlavesForPaths bte = fmap concat . mapM (mkSlaves bte)

buildPathsExplicitly :: BuildTargetEnv -> [FilePath] -> IO BuiltTargets
buildPathsExplicitly bte@BuildTargetEnv{..} paths = do
  built <- waitForSlaves =<< mkSlavesForPaths bte paths
  forM_ paths $ \path ->
    unless (isPhony bteBuildsome path) $
    assertExists path (MissingRule (bsRender bteBuildsome) path bteReason bteParents)
  return built

want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO BuiltTargets
want printer buildsome reason paths = do
  printStrLn printer $
    "Building: " <> ColorText.intercalate ", " (map (cTarget . show) paths)
  let bte =
        BuildTargetEnv
        { bteBuildsome = buildsome
        , btePrinter = printer
        , bteReason = reason
        , bteParents = []
        }
  (buildTime, builtTargets) <- timeIt $ buildPathsExplicitly bte paths
  let stdErrs = Slave.statsStdErr $ builtStats builtTargets
      lastLinePrefix
        | not (S.null stdErrs) =
          cWarning $ "Build Successful, but with STDERR from: " <>
          (cTarget . show . map BuildMaps.targetRepPath . S.toList) stdErrs
        | otherwise =
          cSuccess "Build Successful"
  printStrLn printer $ mconcat
    [ lastLinePrefix, ": "
    , cTiming (show buildTime <> " seconds"), " total." ]
  return builtTargets
  where
    Color.Scheme{..} = Color.scheme

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- FilePath.exists path
  unless doesExist $ E.throwIO err

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

showParents :: Parents -> ColorText
showParents = mconcat . map showParent
  where
    showParent (targetRep, reason) =
      mconcat ["\n-> ", cTarget (show targetRep), " (", reason, ")"]
    Color.Scheme{..} = Color.scheme

data MissingRule = MissingRule (ColorText -> ByteString) FilePath Reason Parents
  deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule render path reason parents) =
    BS8.unpack $ render $ cError $ mconcat
    [ "ERROR: No rule to build ", cTarget (show path), " (", reason, "), needed by:\n"
    , showParents parents
    ]
    where
      Color.Scheme{..} = Color.scheme

data TargetNotCreated = TargetNotCreated (ColorText -> ByteString) FilePath Target
  deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated render path target) =
    BS8.unpack $ render $ cError $ mconcat
    [ (fromString . showPos . targetPos) target
    , ": "
    , cTarget (show path)
    , " explicitly demanded but was not created by its target rule" ]
    where
      Color.Scheme{..} = Color.scheme

isPhony :: Buildsome -> FilePath -> Bool
isPhony bs path = path `S.member` bsPhoniesSet bs

targetIsPhony :: Buildsome -> Target -> Bool
targetIsPhony bs = all (isPhony bs) . targetOutputs

mkSlavesDirectAccess :: BuildTargetEnv -> FilePath -> IO [Slave]
mkSlavesDirectAccess bte@BuildTargetEnv{..} path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing ->
    return []
  Just (targetRep, _targetKind, target) ->
    (: []) <$> getSlaveForTarget bte (TargetDesc targetRep target)

makeChildSlaves :: BuildTargetEnv -> FilePath -> IO [Slave]
makeChildSlaves bte@BuildTargetEnv{..} path
  | not (null childPatterns) =
    fail $ "UNSUPPORTED: Read directory on directory with patterns: " ++ show path ++ " (" ++ BS8.unpack (bsRender bteBuildsome bteReason) ++ ")"
  | otherwise =
    traverse (getSlaveForTarget bte) $
    filter (not . targetIsPhony bteBuildsome . tdTarget) childTargetDescs
  where
    childTargetDescs = map (uncurry TargetDesc) childTargets
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps bteBuildsome) path

mkSlavesForAccessType ::
  FSHook.AccessType -> BuildTargetEnv -> FilePath -> IO [Slave]
mkSlavesForAccessType FSHook.AccessTypeFull = mkSlaves
mkSlavesForAccessType FSHook.AccessTypeModeOnly = mkSlavesDirectAccess
mkSlavesForAccessType FSHook.AccessTypeStat =
  -- This is a (necessary) hack! See KNOWN_ISSUES: "stat of directories"
  mkSlavesDirectAccess

mkSlaves :: BuildTargetEnv -> FilePath -> IO [Slave]
mkSlaves bte@BuildTargetEnv{..} path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise = do
  slaves <- mkSlavesDirectAccess bte path
  childs <- makeChildSlaves bte { bteReason = bteReason <> " (Container directory)" } path
  return $ slaves ++ childs

data IllegalUnspecifiedOutputs = IllegalUnspecifiedOutputs (ColorText -> ByteString) Target [FilePath]
  deriving (Typeable)
instance E.Exception IllegalUnspecifiedOutputs
instance Show IllegalUnspecifiedOutputs where
  show (IllegalUnspecifiedOutputs render target illegalOutputs) =
    BS8.unpack $ render $ cError $ mconcat
    [ "Target: ", cTarget (show (targetOutputs target))
    , " wrote to unspecified output files: ", show illegalOutputs ]
    where
      Color.Scheme{..} = Color.scheme

-- Verify output of whole of slave/execution log
verifyTargetSpec ::
  BuildTargetEnv -> Set FilePath -> Map FilePath KeepsOldContent -> Target ->
  IO (Map FilePath KeepsOldContent)
verifyTargetSpec bte inputs outputs target = do
  verifyTargetInputs bte inputs target
  verifyTargetOutputs bte outputs target

verifyTargetInputs :: BuildTargetEnv -> Set FilePath -> Target -> IO ()
verifyTargetInputs bte@BuildTargetEnv{..} inputs target
  | all (`S.member` bsPhoniesSet bteBuildsome)
    (targetOutputs target) = return () -- Phony target doesn't need real inputs
  | otherwise =
    warnOverSpecified bte "inputs" ""
    (S.fromList (targetAllInputs target) `S.difference`
     (inputs `S.union` bsPhoniesSet bteBuildsome))
    (targetPos target)

handleLegalUnspecifiedOutputs :: BuildTargetEnv -> TargetType output input -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs BuildTargetEnv{..} target unspecifiedOutputs = do
  legalUnspecified <- filterM FilePath.exists unspecifiedOutputs
  unless (null legalUnspecified) $ Print.warn btePrinter (targetPos target) $
    mconcat ["Leaked unspecified outputs: ", show legalUnspecified]
  registerLeakedOutputs bteBuildsome $ S.fromList legalUnspecified

verifyTargetOutputs :: BuildTargetEnv -> Map FilePath KeepsOldContent -> Target -> IO (Map FilePath KeepsOldContent)
verifyTargetOutputs bte@BuildTargetEnv{..} outputs target = do
  handleLegalUnspecifiedOutputs bte target $ M.keys unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <-
    LibMap.filterAWithKey (\path _ -> FilePath.exists path) illegalOutputs
  unless (M.null existingIllegalOutputs) $ do
    Print.posMessage btePrinter (targetPos target) $ cError $
      "Illegal output files: " <> show (M.keys existingIllegalOutputs)

    E.throwIO $
      IllegalUnspecifiedOutputs (bsRender bteBuildsome) target $
      M.keys existingIllegalOutputs
  let unusedOutputs =
        M.keysSet specified `S.difference`
        (M.keysSet outputs `S.union` bsPhoniesSet bteBuildsome)
  warnOverSpecified bte "outputs" " (consider adding a .PHONY declaration)" unusedOutputs (targetPos target)
  mapM_ removeFileOrDirectoryOrNothing $ S.toList unusedOutputs
  return $ M.intersection outputs specified
  where
    Color.Scheme{..} = Color.scheme
    (unspecifiedOutputs, illegalOutputs) =
      M.partitionWithKey (\path _ -> MagicFiles.allowedUnspecifiedOutput path)
      allUnspecified
    allUnspecified = outputs `M.difference` specified
    specified = M.fromSet (const ()) $ S.fromList $ targetOutputs target

warnOverSpecified ::
  BuildTargetEnv -> ColorText -> ColorText ->
  Set FilePath -> SourcePos -> IO ()
warnOverSpecified BuildTargetEnv{..} str suffix unused pos =
  unless (S.null unused) $
  Print.warn btePrinter pos $ mconcat
  ["Over-specified ", str, ": ", cTarget (show (S.toList unused)), suffix]
  where
    Color.Scheme{..} = Color.scheme

replayExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  StdOutputs ByteString -> DiffTime -> IO ()
replayExecutionLog bte@BuildTargetEnv{..} target inputs outputs stdOutputs selfTime =
  Print.replay btePrinter target stdOutputs
  (optVerbosity (bsOpts bteBuildsome)) selfTime $
  -- We only register legal outputs that were CREATED properly in the
  -- execution log, so all outputs keep no old content
  void $ verifyTargetSpec bte inputs
  (M.fromSet (const KeepsNoOldContent) outputs) target

waitForSlavesWithParReleased ::
  Parallelism.Priority -> Parallelism.Cell -> Buildsome ->
  [Slave] -> IO BuiltTargets
waitForSlavesWithParReleased _ _ _ [] = return mempty
waitForSlavesWithParReleased priority parCell buildsome slaves =
  Parallelism.withReleased priority parCell (bsParallelism buildsome) $
  waitForSlaves slaves

waitForSlaves :: [Slave] -> IO BuiltTargets
waitForSlaves slaves =
  do  stats <- mconcat <$> mapM Slave.wait slaves
      return BuiltTargets { builtTargets = map Slave.target slaves, builtStats = stats }

verifyFileDesc ::
  (IsString str, Monoid str, MonadIO m) =>
  str -> FilePath -> Db.FileDesc ne desc ->
  (Posix.FileStatus -> desc -> EitherT (str, FilePath) m ()) ->
  EitherT (str, FilePath) m ()
verifyFileDesc str filePath fileDesc existingVerify = do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case (mStat, fileDesc) of
    (Nothing, Db.FileDescNonExisting _) -> return ()
    (Just stat, Db.FileDescExisting desc) -> existingVerify stat desc
    (Just _, Db.FileDescNonExisting _)  -> left (str <> " file did not exist, now exists", filePath)
    (Nothing, Db.FileDescExisting {}) -> left (str <> " file was deleted", filePath)

data TargetDesc = TargetDesc
  { tdRep :: TargetRep
  , tdTarget :: Target
  }

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Cell ->
  TargetDesc -> Db.ExecutionLog ->
  IO (Either (ByteString, FilePath) (Db.ExecutionLog, BuiltTargets))
tryApplyExecutionLog bte@BuildTargetEnv{..} parCell TargetDesc{..} executionLog@Db.ExecutionLog {..} = do
  builtTargets <- executionLogWaitForInputs bte parCell tdTarget executionLog
  runEitherT $ do
    forM_ (M.toList elInputsDescs) $ \(filePath, desc) ->
      verifyFileDesc "input" filePath desc $ \stat (mtime, Db.InputDesc mModeAccess mStatAccess mContentAccess) -> do
        when (Posix.modificationTimeHiRes stat /= mtime) $ do
          let verify str getDesc mPair = verifyMDesc ("input(" <> str <> ")") filePath getDesc $ snd <$> mPair
          verify "mode" (return (fileModeDescOfStat stat)) mModeAccess
          verify "stat" (return (fileStatDescOfStat stat)) mStatAccess
          verify "content" (fileContentDescOfStat db filePath stat) mContentAccess
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    forM_ (M.toList elOutputsDescs) $ \(filePath, outputDesc) ->
      verifyFileDesc "output" filePath outputDesc $ \stat (Db.OutputDesc oldStatDesc oldMContentDesc) -> do
        verifyDesc  "output(stat)"    filePath (return (fileStatDescOfStat stat)) oldStatDesc
        verifyMDesc "output(content)" filePath (fileContentDescOfStat db filePath stat) oldMContentDesc
    liftIO $
      replayExecutionLog bte tdTarget
      (M.keysSet elInputsDescs) (M.keysSet elOutputsDescs)
      elStdoutputs elSelfTime
    return (executionLog, builtTargets)
  where
    db = bsDb bteBuildsome
    verifyMDesc _   _        _       Nothing        = return ()
    verifyMDesc str filePath getDesc (Just oldDesc) =
      verifyDesc str filePath getDesc oldDesc

    verifyDesc str filePath getDesc oldDesc = do
      newDesc <- liftIO getDesc
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation

executionLogWaitForInputs :: BuildTargetEnv -> Parallelism.Cell -> Target -> Db.ExecutionLog -> IO BuiltTargets
executionLogWaitForInputs bte@BuildTargetEnv{..} parCell target Db.ExecutionLog {..} = do
  -- TODO: This is good for parallelism, but bad if the set of
  -- inputs changed, as it may build stuff that's no longer
  -- required:
  speculativeSlaves <- concat <$> mapM mkInputSlave (M.toList elInputsDescs)
  waitForSlavesWithParReleased Parallelism.PriorityLow parCell bteBuildsome speculativeSlaves
  where
    Color.Scheme{..} = Color.scheme
    hinted = S.fromList $ targetAllInputs target
    mkInputSlave (inputPath, desc)
      | inputPath `S.member` hinted = return []
      | otherwise = mkInputSlaveFor desc inputPath
    mkInputSlaveFor (Db.FileDescNonExisting depReason) =
      mkSlavesDirectAccess bte { bteReason = depReason }
    mkInputSlaveFor (Db.FileDescExisting (_mtime, inputDesc)) =
      case inputDesc of
      Db.InputDesc { Db.idContentAccess = Just (depReason, _) } -> mkSlaves bte { bteReason = depReason }
      Db.InputDesc { Db.idStatAccess = Just (depReason, _) } -> mkSlavesDirectAccess bte { bteReason = depReason }
      Db.InputDesc { Db.idModeAccess = Just (depReason, _) } -> mkSlavesDirectAccess bte { bteReason = depReason }
      Db.InputDesc Nothing Nothing Nothing -> const $ return []

buildParentDirectories ::
  Parallelism.Priority -> BuildTargetEnv -> Parallelism.Cell ->
  [FilePath] -> IO BuiltTargets
buildParentDirectories priority bte@BuildTargetEnv{..} parCell =
  waitForSlavesWithParReleased priority parCell bteBuildsome . concat <=<
  mapM mkParentSlaves . filter (`notElem` ["", "/"])
  where
    Color.Scheme{..} = Color.scheme
    mkParentSlaves path =
      mkSlavesDirectAccess bte
      { bteReason = "Container directory of: " <> cTarget (show path)
      } (FilePath.takeDirectory path)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: BuildTargetEnv -> Parallelism.Cell -> TargetDesc -> IO (Maybe (Db.ExecutionLog, BuiltTargets))
findApplyExecutionLog bte@BuildTargetEnv{..} parCell TargetDesc{..} = do
  mExecutionLog <- readIRef $ Db.executionLog tdTarget $ bsDb bteBuildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return Nothing
    Just executionLog -> do
      eRes <- tryApplyExecutionLog bte parCell TargetDesc{..} executionLog
      case eRes of
        Left (str, filePath) -> do
          printStrLn btePrinter $ bsRender bteBuildsome $ mconcat
            [ "Execution log of ", cTarget (show (targetOutputs tdTarget))
            , " did not match because ", fromBytestring8 str, ": "
            , cPath (show filePath)
            ]
          return Nothing
        Right res -> return (Just res)
  where
    Color.Scheme{..} = Color.scheme

data TargetDependencyLoop = TargetDependencyLoop (ColorText -> ByteString) Parents
  deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop render parents) =
    BS8.unpack $ render $ cError $ fromBytestring8 $
    "Target loop: " <> render (showParents parents)
    where
      Color.Scheme{..} = Color.scheme

data PanicError = PanicError (ColorText -> ByteString) String deriving (Typeable)
instance E.Exception PanicError
instance Show PanicError where
  show (PanicError render msg) =
    BS8.unpack $ render $ cError $ fromString $ "PANIC: " ++ msg
    where
      Color.Scheme{..} = Color.scheme

panic :: (ColorText -> ByteString) -> String -> IO a
panic render msg = do
  IO.hPutStrLn IO.stderr msg
  E.throwIO $ PanicError render msg

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: BuildTargetEnv -> TargetDesc -> IO Slave
getSlaveForTarget bte@BuildTargetEnv{..} TargetDesc{..}
  | any ((== tdRep) . fst) bteParents =
    E.throwIO $ TargetDependencyLoop (bsRender bteBuildsome) newParents
  | otherwise = do
    newSlaveMVar <- newEmptyMVar
    E.mask $ \restoreMask -> join $
      atomicModifyIORef (bsSlaveByTargetRep bteBuildsome) $
      \oldSlaveMap ->
      -- TODO: Use a faster method to lookup&insert at the same time
      case M.lookup tdRep oldSlaveMap of
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
      Nothing ->
        ( M.insert tdRep newSlaveMVar oldSlaveMap
        , mkSlave newSlaveMVar $ \printer allocParCell ->
          allocParCell $ \parCell -> restoreMask $ do
            let newBte = bte { bteParents = newParents, btePrinter = printer }
            buildTarget newBte parCell TargetDesc{..}
        )
    where
      Color.Scheme{..} = Color.scheme
      annotate =
        annotateException $ BS8.unpack $ bsRender bteBuildsome $
        "build failure of " <> cTarget (show (targetOutputs tdTarget)) <> ":\n"
      newParents = (tdRep, bteReason) : bteParents
      panicHandler e@E.SomeException {} =
        panic (bsRender bteBuildsome) $ "FAILED during making of slave: " ++ show e
      mkSlave mvar action = do
        slave <-
          E.handle panicHandler $ do
            allocParCell <-
              Parallelism.startAlloc Parallelism.PriorityLow $
              bsParallelism bteBuildsome
            depPrinterId <- Fresh.next $ bsFreshPrinterIds bteBuildsome
            depPrinter <- Printer.newFrom btePrinter depPrinterId
            Slave.new tdTarget depPrinterId (targetOutputs tdTarget) $
              annotate $ action depPrinter allocParCell
        putMVar mvar slave
        return slave

data UnregisteredOutputFileExists = UnregisteredOutputFileExists (ColorText -> ByteString) FilePath
  deriving (Typeable)
instance E.Exception UnregisteredOutputFileExists
instance Show UnregisteredOutputFileExists where
  show (UnregisteredOutputFileExists render path) =
    BS8.unpack $ render $ cError $ mconcat
    [ cTarget (show path), " specified as output but exists as a file that "
    , "was not created by buildsome (use --overwrite to go ahead "
    , "anyway)" ]
    where
      Color.Scheme{..} = Color.scheme

removeOldUnregisteredOutput :: Printer -> Buildsome -> FilePath -> IO ()
removeOldUnregisteredOutput printer buildsome path =
  case optOverwriteUnregisteredOutputs (bsOpts buildsome) of
  Opts.DontOverwriteUnregisteredOutputs ->
    E.throwIO $ UnregisteredOutputFileExists (bsRender buildsome) path
  Opts.OverwriteUnregisteredOutputs -> do
    Printer.printStrLn printer $
      "Overwriting " <> cTarget (show path) <> " (due to --overwrite)"
    removeFileOrDirectory path
  where
    Color.Scheme{..} = Color.scheme

removeOldOutput :: Printer -> Buildsome -> Set FilePath -> FilePath -> IO ()
removeOldOutput printer buildsome registeredOutputs path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> return () -- Nothing to do
    Just _
      | path `S.member` registeredOutputs -> removeFileOrDirectory path
      | otherwise -> removeOldUnregisteredOutput printer buildsome path

data RunCmdResults = RunCmdResults
  { rcrSelfTime :: DiffTime -- excluding pause times
  , rcrStdOutputs :: StdOutputs ByteString
  , rcrInputs :: Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)
  , rcrOutputs :: Map FilePath (KeepsOldContent, Reason)
  , rcrBuiltTargets :: BuiltTargets
  }

outEffectToMaybe :: OutputEffect -> Maybe KeepsOldContent
outEffectToMaybe OutputEffectNone = Nothing
outEffectToMaybe (OutputEffectChanged keepsOld) = Just keepsOld

delayedOutputEffect :: FSHook.DelayedOutput -> IO (Maybe KeepsOldContent)
delayedOutputEffect (FSHook.DelayedOutput behavior path) = do
  outputExists <- FilePath.exists path
  return $ outEffectToMaybe $
    if outputExists
    then whenFileExists behavior
    else whenFileMissing behavior

undelayedOutputEffect :: FSHook.OutFilePath -> Maybe KeepsOldContent
undelayedOutputEffect (FSHook.OutFilePath _ effect) =
  case effect of
  FSHook.OutEffectNothing -> Nothing
  FSHook.OutEffectCreated -> Just KeepsNoOldContent
  FSHook.OutEffectDeleted -> Just KeepsNoOldContent
  FSHook.OutEffectChanged -> Just KeepsOldContent
  FSHook.OutEffectUnknown -> Just KeepsOldContent -- conservatively assume content kept

recordInput ::
  IORef (Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)) ->
  Reason -> FSHook.Input -> IO ()
recordInput inputsRef reason (FSHook.Input accessType path) = do
  mStat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $ M.insertWith merge path
    (newAccessTypes, mStat)
  where
    newAccessTypes = M.singleton accessType reason
    -- TODO: We'd like to verify no meddling of the create-delete,
    -- which would go undetected, lacking mtime via:

    -- merge (_, Just _) (_, Nothing) = ...

    -- However, we must then verify that it wasn't a legal output file
    -- of the same job
    merge _ (oldAccessTypes, oldMStat) = (oldAccessTypes `mappend` newAccessTypes, oldMStat)

recordOutputs ::
  Buildsome ->
  IORef (Map FilePath (KeepsOldContent, Reason)) -> Reason ->
  Set FilePath -> Map FilePath KeepsOldContent -> IO ()
recordOutputs buildsome outputsRef accessDoc targetOutputsSet paths = do
  atomicModifyIORef'_ outputsRef $ M.unionWith combineOutputs $ M.fromList
    [ (path, (keepsOldContent, accessDoc))
    | (path, keepsOldContent) <- M.toList paths
    ]
  registerOutputs buildsome $ M.keysSet paths `S.intersection` targetOutputsSet
  where
    combineOutputs a@(ax, _) b@(bx, _)
      -- KeepsNoOldContent < KeepsOldContent
      | ax < bx = a
      | otherwise = b

makeInputs ::
  IORef BuiltTargets -> BuildTargetEnv -> Parallelism.Cell -> Reason ->
  [FSHook.Input] -> [FSHook.DelayedOutput] -> IO ()
makeInputs builtTargetsRef bte@BuildTargetEnv{..} parCell accessDoc inputs outputs =
  do
    let allPaths = map FSHook.inputPath inputs ++ map FSHook.outputPath outputs
    parentBuiltTargets <-
      buildParentDirectories Parallelism.PriorityHigh bte parCell
      allPaths
    slaves <-
      fmap concat $ forM inputs $ \(FSHook.Input accessType path) ->
      mkSlavesForAccessType accessType bte { bteReason = accessDoc } path
    mappendBuiltTargets . mappend parentBuiltTargets =<<
      waitForSlavesWithParReleased Parallelism.PriorityHigh parCell bteBuildsome
      slaves
    where
      mappendBuiltTargets x = atomicModifyIORef_ builtTargetsRef (mappend x)

fsAccessHandlers ::
  IORef (Map FilePath (KeepsOldContent, Reason)) ->
  IORef (Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)) ->
  IORef BuiltTargets ->
  BuildTargetEnv ->
  Parallelism.Cell ->
  TargetType FilePath input ->
  FSHook.FSAccessHandlers
fsAccessHandlers outputsRef inputsRef builtTargetsRef bte@BuildTargetEnv{..}
  parCell target =
    FSHook.FSAccessHandlers
    { delayedFSAccessHandler = fsDelayedAccessHandler
    , undelayedFSAccessHandler = fsUndelayedAccessHandler
    }
  where
    fsUndelayedAccessHandler accessDoc rawInputs rawOutputs = do
      commonAccessHandler accessDoc rawInputs rawOutputs
        FSHook.outPath (return . undelayedOutputEffect) $ \_ _ -> return ()

    fsDelayedAccessHandler accessDoc rawInputs rawOutputs = do
      commonAccessHandler accessDoc rawInputs rawOutputs
        FSHook.outputPath delayedOutputEffect $ makeInputs builtTargetsRef bte parCell accessDoc

    targetOutputsSet = S.fromList $ targetOutputs target

    inputIgnored recordedOutputs (FSHook.Input _ path) =
      MagicFiles.inputIgnored path ||
      path `M.member` recordedOutputs ||
      path `S.member` targetOutputsSet

    commonAccessHandler accessDoc rawInputs rawOutputs
      getOutPath getOutEffect handler = do
        recordedOutputs <- readIORef outputsRef
        let ignoredOutput = MagicFiles.outputIgnored . getOutPath
            ignoredInput = inputIgnored recordedOutputs
            inputs = filter (not . ignoredInput) rawInputs
            outputs = filter (not . ignoredOutput) rawOutputs
        () <- handler inputs outputs
        let addMEffect output = do
              mEffect <- getOutEffect output
              return $ (,) (getOutPath output) <$> mEffect
        filteredOutputs <- fmap (M.fromList . catMaybes) $ mapM addMEffect outputs
        recordOutputs bteBuildsome outputsRef accessDoc
          targetOutputsSet filteredOutputs
        mapM_ (recordInput inputsRef accessDoc) $
          filter ((`M.notMember` recordedOutputs) . FSHook.inputPath) inputs

runCmd :: BuildTargetEnv -> Parallelism.Cell -> Target -> IO RunCmdResults
runCmd bte@BuildTargetEnv{..} parCell target = do
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  builtTargetsRef <- newIORef mempty
  let accessHandlers = fsAccessHandlers outputsRef inputsRef builtTargetsRef bte parCell target
  (time, stdOutputs) <-
    FSHook.timedRunCommand hook rootPath shellCmd
    renderedTargetOutputs accessHandlers
  inputs <- readIORef inputsRef
  outputs <- readIORef outputsRef
  builtTargets <- readIORef builtTargetsRef
  return RunCmdResults
    { rcrStdOutputs = stdOutputs
    , rcrSelfTime = realToFrac time
    , rcrInputs =
      -- This is because we don't serialize input/output access
      -- (especially when undelayed!) to the same file. So we don't
      -- really have a correct input stat/desc here for such inputs. However, outputs should always considered as mode-only inputs.
      inputs `M.difference` outputs
    , rcrOutputs = outputs
    , rcrBuiltTargets = builtTargets
    }
  where
    rootPath = bsRootPath bteBuildsome
    hook = bsFsHook bteBuildsome
    renderedTargetOutputs = cTarget $ show $ targetOutputs target
    shellCmd = shellCmdVerify bte target ["HOME", "PATH"]
    Color.Scheme{..} = Color.scheme

makeExecutionLog ::
  Buildsome -> Target ->
  Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
  [FilePath] -> StdOutputs ByteString -> DiffTime -> IO Db.ExecutionLog
makeExecutionLog buildsome target inputs outputs stdOutputs selfTime = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM outputs $ \outPath -> do
      mStat <- Dir.getMFileStatus outPath
      fileDesc <-
        case mStat of
        Nothing -> return $ Db.FileDescNonExisting ()
        Just stat -> do
          mContentDesc <-
            if Posix.isDirectory stat
            then return Nothing
            else Just <$> fileContentDescOfStat db outPath stat
          return $ Db.FileDescExisting $ Db.OutputDesc (fileStatDescOfStat stat) mContentDesc
      return (outPath, fileDesc)
  return Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elCommand = targetCmds target
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = stdOutputs
    , elSelfTime = selfTime
    }
  where
    db = bsDb buildsome
    assertFileMTime path oldMStat =
      unless (MagicFiles.allowedUnspecifiedOutput path) $
      Meddling.assertFileMTime path oldMStat
    inputAccess ::
      FilePath ->
      (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
      IO (Db.FileDesc Reason (POSIXTime, Db.InputDesc))
    inputAccess path (accessTypes, Nothing) = do
      let reason =
            case M.elems accessTypes of
            [] -> error $ "AccessTypes empty in rcrInputs:" ++ show path
            x:_ -> x
      assertFileMTime path Nothing
      return $ Db.FileDescNonExisting reason
    inputAccess path (accessTypes, Just stat) = do
      assertFileMTime path $ Just stat
      let
        addDesc accessType getDesc = do
          desc <- getDesc
          return $ flip (,) desc <$> M.lookup accessType accessTypes
      modeAccess <-
        addDesc FSHook.AccessTypeModeOnly $ return $ fileModeDescOfStat stat
      statAccess <-
        addDesc FSHook.AccessTypeStat $ return $ fileStatDescOfStat stat
      contentAccess <-
        addDesc FSHook.AccessTypeFull $ fileContentDescOfStat db path stat
      return $ Db.FileDescExisting
        ( Posix.modificationTimeHiRes stat
        , Db.InputDesc
          { Db.idModeAccess = modeAccess
          , Db.idStatAccess = statAccess
          , Db.idContentAccess = contentAccess
          }
        )

redirectExceptions :: Buildsome -> IO a -> IO a
redirectExceptions buildsome =
  handleSync $ \e@E.SomeException {} -> do
    bsFastKillBuild buildsome e
    E.throwIO e

deleteOldTargetOutputs :: BuildTargetEnv -> TargetType FilePath input -> IO ()
deleteOldTargetOutputs BuildTargetEnv{..} target = do
  registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb bteBuildsome
  mapM_ (removeOldOutput btePrinter bteBuildsome registeredOutputs) $
    targetOutputs target

buildTargetHints ::
  BuildTargetEnv -> Parallelism.Cell -> TargetType FilePath FilePath -> IO BuiltTargets
buildTargetHints bte@BuildTargetEnv{..} parCell target = do
  targetParentsBuilt <-
    buildParentDirectories Parallelism.PriorityLow bte parCell $
    targetOutputs target

  inputsBuilt <-
    Parallelism.withReleased Parallelism.PriorityLow parCell (bsParallelism bteBuildsome) $
    buildPathsExplicitly
      bte { bteReason = "Hint from " <> cTarget (show (targetOutputs target)) } $
      targetAllInputs target

  return $ targetParentsBuilt <> inputsBuilt
  where
    Color.Scheme{..} = Color.scheme

buildTargetReal ::
  BuildTargetEnv -> Parallelism.Cell -> TargetDesc -> IO (Db.ExecutionLog, BuiltTargets)
buildTargetReal bte@BuildTargetEnv{..} parCell TargetDesc{..} =
  Print.targetWrap btePrinter bteReason tdTarget "BUILDING" $ do
    deleteOldTargetOutputs bte tdTarget

    Print.executionCmd verbosityCommands btePrinter tdTarget

    RunCmdResults{..} <- runCmd bte parCell tdTarget

    outputs <- verifyTargetSpec bte (M.keysSet rcrInputs) (M.map fst rcrOutputs) tdTarget
    executionLog <- makeExecutionLog bteBuildsome tdTarget rcrInputs (M.keys outputs) rcrStdOutputs rcrSelfTime
    writeIRef (Db.executionLog tdTarget (bsDb bteBuildsome)) executionLog

    Print.targetTiming btePrinter "now" rcrSelfTime
    return (executionLog, rcrBuiltTargets)
  where
    verbosityCommands = Opts.verbosityCommands verbosity
    verbosity = optVerbosity (bsOpts bteBuildsome)

buildTarget :: BuildTargetEnv -> Parallelism.Cell -> TargetDesc -> IO Slave.Stats
buildTarget bte@BuildTargetEnv{..} parCell TargetDesc{..} =
  redirectExceptions bteBuildsome $ do
    hintedBuiltTargets <- buildTargetHints bte parCell tdTarget
    mSlaveStats <- findApplyExecutionLog bte parCell TargetDesc{..}
    (whenBuilt, (Db.ExecutionLog{..}, builtTargets)) <-
      case mSlaveStats of
      Just res -> return (Slave.FromCache, res)
      Nothing -> (,) Slave.BuiltNow <$> buildTargetReal bte parCell TargetDesc{..}
    let BuiltTargets deps stats = hintedBuiltTargets <> builtTargets
    return $ stats <>
      Slave.Stats
      { Slave.statsOfTarget = M.singleton tdRep (whenBuilt, elSelfTime, deps)
      , Slave.statsStdErr =
        if mempty /= stdErr elStdoutputs
        then S.singleton tdRep
        else S.empty
      }
  where
    Color.Scheme{..} = Color.scheme

registerDbList :: Ord a => (Db -> IORef (Set a)) -> Buildsome -> Set a -> IO ()
registerDbList mkIORef buildsome newItems =
  atomicModifyIORef'_ (mkIORef (bsDb buildsome)) (newItems <>)

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs = registerDbList Db.registeredOutputsRef

registerLeakedOutputs :: Buildsome -> Set FilePath -> IO ()
registerLeakedOutputs = registerDbList Db.leakedOutputsRef

deleteRemovedOutputs :: Printer -> Db -> BuildMaps -> IO ()
deleteRemovedOutputs printer db buildMaps = do
  toDelete <-
    atomicModifyIORef' (Db.registeredOutputsRef db) $
    S.partition (isJust . BuildMaps.find buildMaps)
  forM_ (S.toList toDelete) $ \path -> do
    Printer.rawPrintStrLn printer $ "Removing old output: " <> cPath (show path)
    removeFileOrDirectoryOrNothing path
  where
    Color.Scheme{..} = Color.scheme

data TargetCommandFailed = TargetCommandFailed (ColorText -> ByteString) Target ExitCode (StdOutputs ByteString) deriving (Typeable)
instance E.Exception TargetCommandFailed
instance Show TargetCommandFailed where
  show (TargetCommandFailed render target exitCode stdOutputs) =
    BS8.unpack $ render $ cError $ mconcat
    [ fromBytestring8 (Print.delimitMultiline cmd), " failed: ", show exitCode
    , maybe "" ("\n" <>) $ Print.outputsStr stdOutputs ]
    where
      Color.Scheme{..} = Color.scheme
      cmd = targetCmds target

shellCmdVerify ::
  BuildTargetEnv -> Target -> [String] -> Process.Env -> IO (StdOutputs ByteString)
shellCmdVerify BuildTargetEnv{..} target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand (BS8.unpack (targetCmds target))) inheritEnvs newEnvs
  let stdOutputs = StdOutputs stdout stderr
  case exitCode of
    ExitFailure {} ->
      E.throwIO $ TargetCommandFailed (bsRender bteBuildsome) target exitCode stdOutputs
    _ -> return ()
  Print.targetStdOutputs btePrinter target stdOutputs
  return stdOutputs

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

disallowExceptions :: IO a -> String -> IO a
disallowExceptions act prefix = act `E.catch` \e@E.SomeException {} -> do
  putStrLn $ prefix ++ show e
  E.throwIO e

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb makefilePath = Db.with $ buildDbFilename makefilePath

inKillableThread :: ((E.SomeException -> IO ()) -> IO a) -> IO a
inKillableThread act = do
  a <- async $ do
    t <- myThreadId
    act $ E.throwTo t
  wait a

with ::
  Printer -> Db -> FilePath -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
with printer db makefilePath makefile opt@Opt{..} body = do
  ldPreloadPath <- FSHook.getLdPreloadPath optFsOverrideLdPreloadPath
  FSHook.with printer ldPreloadPath $ \fsHook -> do
    slaveMapByTargetRep <- newIORef M.empty
    parallelism <- Parallelism.new $ fromMaybe 1 optParallelism
    freshPrinterIds <- Fresh.new 1
    buildId <- BuildId.new
    rootPath <- FilePath.canonicalizePath $ FilePath.takeDirectory makefilePath
    let
      buildMaps = BuildMaps.make makefile
    deleteRemovedOutputs printer db buildMaps

    inKillableThread $ \killThread -> do
      killThreadOnce <- (void .) . (. killThread) <$> once
      let
        buildsome =
          Buildsome
          { bsOpts = opt
          , bsMakefile = makefile
          , bsPhoniesSet = S.fromList . map snd $ makefilePhonies makefile
          , bsBuildId = buildId
          , bsRootPath = rootPath
          , bsBuildMaps = buildMaps
          , bsDb = db
          , bsFsHook = fsHook
          , bsSlaveByTargetRep = slaveMapByTargetRep
          , bsParallelism = parallelism
          , bsFreshPrinterIds = freshPrinterIds
          , bsFastKillBuild = case optKeepGoing of
              Opts.KeepGoing -> const (return ())
              Opts.DieQuickly -> killThreadOnce
          , bsRender = Printer.render printer
          }
      body buildsome
        -- We must not leak running slaves as we're not allowed to
        -- access fsHook, db, etc after leaving here:
        `E.onException` putStrLn "Shutting down"
        `finally` (cancelAllSlaves printer buildsome
                   `disallowExceptions` "BUG: Exception thrown at cancelAllSlaves: ")
        -- Must update gitIgnore after all the slaves finished updating
        -- the registered output lists:
        `finally` maybeUpdateGitIgnore buildsome
  where
    maybeUpdateGitIgnore buildsome =
      case optUpdateGitIgnore of
      Opts.UpdateGitIgnore -> updateGitIgnore buildsome makefilePath
      Opts.DontUpdateGitIgnore -> return ()

clean :: Printer -> Buildsome -> IO ()
clean printer buildsome = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  Clean.Result _totalSize totalSpace count <-
    mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
  writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
  writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
  Printer.rawPrintStrLn printer $ mconcat
    [ cSuccess "Clean Successful", ": Cleaned "
    , show count, " files freeing an estimated "
    , showBytes (fromIntegral totalSpace)
    ]
  where
    Color.Scheme{..} = Color.scheme
