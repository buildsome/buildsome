{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Buildsome
  ( Buildsome, with
  , clean, want
  ) where

import Buildsome.Db (Db, IRef(..), Reason)
import Buildsome.FileDescCache (getFileDesc, fileDescOfMStat)
import Buildsome.Opts (Opt(..))
import Buildsome.Print (targetShow)
import Control.Applicative ((<$>), Applicative(..))
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lib.AnnotatedException (annotateException)
import Lib.BuildId (BuildId)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.ColorText (ColorText, renderStr)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing, exists)
import Lib.Exception (finally)
import Lib.FSHook (FSHook)
import Lib.FileDesc (fileModeDescOfMStat, getFileModeDesc, fileStatDescOfMStat, getFileStatDesc)
import Lib.FilePath (FilePath, (</>), (<.>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import Lib.Makefile (Makefile(..), TargetType(..), Target, targetAllInputs)
import Lib.Parallelism (Parallelism)
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
import qualified Buildsome.Opts as Opts
import qualified Buildsome.Print as Print
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Lib.BuildId as BuildId
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.ColorText as ColorText
import qualified Lib.FSHook as FSHook
import qualified Lib.FilePath as FilePath
import qualified Lib.Fresh as Fresh
import qualified Lib.Parallelism as Parallelism
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Slave as Slave
import qualified Lib.Timeout as Timeout
import qualified Prelude
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]

data Buildsome = Buildsome
  { -- static:
    bsOpts :: Opt
  , bsMakefile :: Makefile
  , bsBuildId :: BuildId
  , bsRootPath :: FilePath
  , bsBuildMaps :: BuildMaps
    -- dynamic:
  , bsDb :: Db
  , bsFsHook :: FSHook
  , bsSlaveByTargetRep :: IORef (Map TargetRep (MVar Slave))
  , bsParallelism :: Parallelism
  , bsFreshPrinterIds :: Fresh Printer.Id
  }

cancelAllSlaves :: Buildsome -> IO ()
cancelAllSlaves bs = go 0
  where
    timeoutVerifyCancelled slave =
      Timeout.warning (Timeout.seconds 2)
      (unwords ["Slave", Slave.str slave, "did not cancel in 2 seconds!"]) $
      Slave.cancel slave
    go alreadyCancelled = do
      curSlaveMap <- readIORef $ bsSlaveByTargetRep bs
      slaves <-
        forM (M.toList curSlaveMap) $
        \(targetRep, mvar) ->
        Timeout.warning (Timeout.millis 100)
        (renderStr
         ("Slave MVar for " <> targetShow targetRep <> " not populated in 100 millis!")) $
        readMVar mvar
      let count = length slaves
      unless (alreadyCancelled >= count) $ do
        mapM_ timeoutVerifyCancelled slaves
        -- Make sure to cancel any potential new slaves that were
        -- created during cancellation
        go count

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = FilePath.takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  putStrLn "Updating .gitignore"
  BS8.writeFile (BS8.unpack gitIgnorePath) $ BS8.unlines $
    map (("/" <>) . FilePath.makeRelative dir) $
    extraIgnored ++ S.toList (outputs <> leaked)

data BuildTargetEnv = BuildTargetEnv
  { bteBuildsome :: Buildsome
  , btePrinter :: Printer
  , bteReason :: Reason
  , bteParents :: Parents
  }

mkSlavesForPaths :: BuildTargetEnv -> Explicitness -> [FilePath] -> IO [Slave]
mkSlavesForPaths bte explicitness = fmap concat . mapM (mkSlaves bte explicitness)

want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO Slave.Stats
want printer buildsome reason paths = do
  printStrLn printer $
    "Building: " <> ColorText.intercalate ", " (map targetShow paths)
  (buildTime, slaveStats) <- timeIt $
    fmap mconcat . mapM (Slave.wait printer) =<<
    mkSlavesForPaths BuildTargetEnv
    { bteBuildsome = buildsome
    , btePrinter = printer
    , bteReason = reason
    , bteParents = []
    } Explicit paths
  printStrLn printer $ mconcat
    [ Color.success "Build Successful", ": "
    , Color.timing (show buildTime <> " seconds"), " total." ]
  return slaveStats

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- exists path
  unless doesExist $ E.throwIO err

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

data MissingRule = MissingRule FilePath Reason deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule path reason) =
    renderStr $ Color.error $ mconcat
    ["ERROR: No rule to build ", targetShow path, " (", fromBytestring8 reason, ")"]

data TargetNotCreated = TargetNotCreated FilePath deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated path) =
    renderStr $ Color.error $ mconcat
    [ targetShow path
    , " explicitly demanded but was not created by its target rule" ]

mkSlavesDirectAccess :: BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlavesDirectAccess bte@BuildTargetEnv{..} explicitness path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $ assertExists path $ MissingRule path bteReason
    return []
  Just tgt -> do
    slave <- getSlaveForTarget bte tgt
    (: []) <$> case explicitness of
      Implicit -> return slave
      Explicit -> verifyFileGetsCreated slave
  where
    verifyFileGetsCreated slave
      | path `elem` makefilePhonies (bsMakefile bteBuildsome) = return slave
      | otherwise =
        Slave.wrap (<* assertExists path (TargetNotCreated path)) slave

makeChildSlaves :: BuildTargetEnv -> FilePath -> IO [Slave]
makeChildSlaves bte@BuildTargetEnv{..} path
  | not (null childPatterns) =
    fail "UNSUPPORTED: Read directory on directory with patterns"
  | otherwise =
    traverse (getSlaveForTarget bte)
    childTargets
  where
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps bteBuildsome) path

mkSlavesForAccessType ::
  FSHook.AccessType -> BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlavesForAccessType FSHook.AccessTypeFull = mkSlaves
mkSlavesForAccessType FSHook.AccessTypeModeOnly = mkSlavesDirectAccess
mkSlavesForAccessType FSHook.AccessTypeStat =
  -- This is a (necessary) hack! See KNOWN_ISSUES: "stat of directories"
  mkSlavesDirectAccess

mkSlaves :: BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlaves bte@BuildTargetEnv{..} explicitness path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise = do
  slaves <- mkSlavesDirectAccess bte explicitness path
  childs <- makeChildSlaves bte { bteReason = bteReason <> " (Container directory)" } path
  return $ slaves ++ childs

-- e.g: .pyc files
handleLegalUnspecifiedOutputs :: Buildsome -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs buildsome target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ Print.warn (targetPos target) $
    mconcat ["Leaked unspecified outputs: ", show paths, ", ", actionDesc]
  action
  where
    (actionDesc, action) =
      case optDeleteUnspecifiedOutputs (bsOpts buildsome) of
      Opts.DeleteUnspecifiedOutputs -> ("deleting", mapM_ removeFileOrDirectoryOrNothing paths)
      Opts.DontDeleteUnspecifiedOutputs -> ("keeping", registerLeakedOutputs buildsome (S.fromList paths))

data IllegalUnspecifiedOutputs = IllegalUnspecifiedOutputs Target [FilePath] deriving (Typeable)
instance E.Exception IllegalUnspecifiedOutputs
instance Show IllegalUnspecifiedOutputs where
  show (IllegalUnspecifiedOutputs target illegalOutputs) =
    renderStr $ Color.error $ mconcat
    [ "Target: ", targetShow (targetOutputs target)
    , " wrote to unspecified output files: ", show illegalOutputs ]

-- Verify output of whole of slave/execution log
verifyTargetSpec :: Buildsome -> Set FilePath -> Set FilePath -> Target -> IO ()
verifyTargetSpec buildsome inputs outputs target = do
  verifyTargetInputs buildsome inputs target
  verifyTargetOutputs buildsome outputs target

phoniesSet :: Buildsome -> Set FilePath
phoniesSet = S.fromList . makefilePhonies . bsMakefile

verifyTargetInputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetInputs buildsome inputs target
  | all (`S.member` phoniesSet buildsome)
    (targetOutputs target) = return () -- Phony target doesn't need real inputs
  | otherwise =
    warnOverSpecified "inputs" (S.fromList (targetAllInputs target))
    (inputs `S.union` phoniesSet buildsome) (targetPos target)

verifyTargetOutputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs buildsome outputs target = do
  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs buildsome target =<<
    filterM Posix.fileExist unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM Posix.fileExist illegalOutputs
  unless (null existingIllegalOutputs) $ do
    Print.posMessage (targetPos target) $ Color.error $
      "Illegal output files created: " <> show existingIllegalOutputs

    Print.warn (targetPos target) $ "leaving leaked unspecified output effects" -- Need to make sure we only record actual outputs, and not *attempted* outputs before we delete this
    -- mapM_ removeFileOrDirectory existingIllegalOutputs

    E.throwIO $ IllegalUnspecifiedOutputs target existingIllegalOutputs
  warnOverSpecified "outputs" specified (outputs `S.union` phoniesSet buildsome) (targetPos target)
  where
    (unspecifiedOutputs, illegalOutputs) =
      partition MagicFiles.allowedUnspecifiedOutput allUnspecified
    allUnspecified = S.toList $ outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

warnOverSpecified ::
  ColorText -> Set FilePath -> Set FilePath -> SourcePos -> IO ()
warnOverSpecified str specified used pos =
  unless (S.null unused) $
  Print.warn pos $ mconcat
    ["Over-specified ", str, ": ", targetShow (S.toList unused)]
  where
    unused = specified `S.difference` used

-- Already verified that the execution log is a match
applyExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  StdOutputs ByteString -> DiffTime -> IO ()
applyExecutionLog BuildTargetEnv{..} target inputs outputs stdOutputs selfTime =
  Print.targetWrap btePrinter bteReason target "REPLAY" $ do
    Print.cmd btePrinter target
    Print.targetStdOutputs target stdOutputs
    verifyTargetSpec bteBuildsome inputs outputs target
    printStrLn btePrinter $ ColorText.render $
      "Build (originally) took " <> Color.timing (show selfTime <> " seconds")

waitForSlaves :: Printer -> Parallelism.Cell -> Buildsome -> [Slave] -> IO Slave.Stats
waitForSlaves _ _ _ [] = return mempty
waitForSlaves printer parCell buildsome slaves =
  Parallelism.withReleased parCell (bsParallelism buildsome) $
  mconcat <$> mapM (Slave.wait printer) slaves

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Cell ->
  TargetRep -> Target -> Db.ExecutionLog ->
  IO (Either (ByteString, FilePath) Slave.Stats)
tryApplyExecutionLog bte@BuildTargetEnv{..} parCell targetRep target Db.ExecutionLog {..} = do
  nestedSlaveStats <- waitForInputs
  runEitherT $ do
    forM_ (M.toList elInputsDescs) $ \(filePath, oldInputAccess) ->
      case snd oldInputAccess of
        Db.InputAccessFull     oldDesc     -> compareToNewDesc "input"       (getFileDesc db) (filePath, oldDesc)
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc (filePath, oldModeDesc)
        Db.InputAccessIgnoredFile          -> return ()
        Db.InputAccessStatOnly oldStatDesc -> compareToNewDesc "input(stat)" getFileStatDesc (filePath, oldStatDesc)
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    mapM_ (compareToNewDesc "output" (getFileDesc db)) $ M.toList elOutputsDescs

    liftIO $
      applyExecutionLog bte target
      (M.keysSet elInputsDescs) (M.keysSet elOutputsDescs)
      elStdoutputs elSelfTime
    let selfStats = Slave.Stats $ M.singleton targetRep (Slave.FromCache, elSelfTime)
    return $ mappend selfStats nestedSlaveStats
  where
    db = bsDb bteBuildsome
    compareToNewDesc str getNewDesc (filePath, oldDesc) = do
      newDesc <- liftIO $ getNewDesc filePath
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation
    inputAccessToType Db.InputAccessModeOnly {} = Just FSHook.AccessTypeModeOnly
    inputAccessToType Db.InputAccessStatOnly {} = Just FSHook.AccessTypeStat
    inputAccessToType Db.InputAccessFull {} = Just FSHook.AccessTypeFull
    inputAccessToType Db.InputAccessIgnoredFile = Nothing
    waitForInputs = do
      -- TODO: This is good for parallelism, but bad if the set of
      -- inputs changed, as it may build stuff that's no longer
      -- required:
      let hinted = S.fromList $ targetAllInputs target
      speculativeSlaves <-
        fmap concat $ forM (M.toList elInputsDescs) $ \(inputPath, (depReason, inputAccess)) ->
        if inputPath `S.member` hinted
        then return []
        else case inputAccessToType inputAccess of
          Nothing -> return []
          Just accessType ->
            mkSlavesForAccessType accessType
            bte { bteReason = depReason } Implicit inputPath

      let hintReason = ColorText.render $ "Hint from " <> (targetShow . targetOutputs) target
      hintedSlaves <-
        mkSlavesForPaths bte { bteReason = hintReason } Explicit $ targetAllInputs target

      targetParentsStats <- buildParentDirectories bte parCell Explicit $ targetOutputs target

      let allSlaves = speculativeSlaves ++ hintedSlaves
      mappend targetParentsStats <$>
        waitForSlaves btePrinter parCell bteBuildsome allSlaves

buildParentDirectories :: BuildTargetEnv -> Parallelism.Cell -> Explicitness -> [FilePath] -> IO Slave.Stats
buildParentDirectories bte@BuildTargetEnv{..} parCell explicitness =
  waitForSlaves btePrinter parCell bteBuildsome . concat <=<
  mapM mkParentSlaves . filter (`notElem` ["", "/"])
  where
    mkParentSlaves path =
      mkSlavesDirectAccess bte
      { bteReason =
        ColorText.render $ "Container directory of: " <> targetShow path
      } explicitness (FilePath.takeDirectory path)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: BuildTargetEnv -> Parallelism.Cell -> TargetRep -> Target -> IO (Maybe Slave.Stats)
findApplyExecutionLog bte@BuildTargetEnv{..} parCell targetRep target = do
  mExecutionLog <- readIRef $ Db.executionLog target $ bsDb bteBuildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return Nothing
    Just executionLog -> do
      res <- tryApplyExecutionLog bte parCell targetRep target executionLog
      case res of
        Left (str, filePath) -> do
          printStrLn btePrinter $ ColorText.render $ mconcat
            [ "Execution log of ", targetShow (targetOutputs target)
            , " did not match because ", fromBytestring8 str, ": "
            , Color.path (show filePath), " changed"
            ]
          return Nothing
        Right stats -> return (Just stats)

showParents :: Parents -> ByteString
showParents = ColorText.render . mconcat . map showParent
  where
    showParent (targetRep, reason) =
      mconcat ["\n-> ", targetShow targetRep, " (", fromBytestring8 reason, ")"]

data TargetDependencyLoop = TargetDependencyLoop Parents deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop parents) =
    renderStr $ Color.error $ fromBytestring8 $ "Target loop: " <> showParents parents

data PanicError = PanicError String deriving (Typeable)
instance E.Exception PanicError
instance Show PanicError where
  show (PanicError msg) =
    renderStr $ Color.error $ fromString $ "PANIC: " ++ msg

panic :: String -> IO a
panic msg = do
  IO.hPutStrLn IO.stderr msg
  E.throwIO $ PanicError msg

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: BuildTargetEnv -> (TargetRep, Target) -> IO Slave
getSlaveForTarget bte@BuildTargetEnv{..} (targetRep, target)
  | any ((== targetRep) . fst) bteParents = E.throwIO $ TargetDependencyLoop newParents
  | otherwise = do
    newSlaveMVar <- newEmptyMVar
    E.mask $ \restoreMask -> join $
      atomicModifyIORef (bsSlaveByTargetRep bteBuildsome) $
      \oldSlaveMap ->
      -- TODO: Use a faster method to lookup&insert at the same time
      case M.lookup targetRep oldSlaveMap of
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
      Nothing ->
        ( M.insert targetRep newSlaveMVar oldSlaveMap
        , mkSlave newSlaveMVar $ \printer getParId ->
          E.bracket (Parallelism.newCell =<< getParId) (Parallelism.release par) $
          \parCell -> restoreMask $ do
            let newBte = bte { bteParents = newParents, btePrinter = printer }
            buildTarget newBte parCell targetRep target
        )
    where
      par = bsParallelism bteBuildsome
      annotate =
        annotateException $ renderStr $
        "build failure of " <> targetShow (targetOutputs target) <> ":\n"
      newParents = (targetRep, bteReason) : bteParents
      panicHandler e@E.SomeException {} = panic $ "FAILED during making of slave: " ++ show e
      mkSlave mvar action = do
        slave <-
          E.handle panicHandler $ do
            getParId <- Parallelism.startAlloc $ bsParallelism bteBuildsome
            depPrinterId <- Fresh.next $ bsFreshPrinterIds bteBuildsome
            depPrinter <- Printer.newFrom btePrinter depPrinterId
            Slave.new depPrinterId (targetOutputs target) $ annotate $ action depPrinter getParId
        putMVar mvar slave
        return slave

data UnregisteredOutputFileExists = UnregisteredOutputFileExists FilePath deriving (Typeable)
instance E.Exception UnregisteredOutputFileExists
instance Show UnregisteredOutputFileExists where
  show (UnregisteredOutputFileExists path) =
    renderStr $ Color.error $ mconcat
    [ targetShow path, " specified as output but exists as a file that "
    , "was not created by buildsome (use --overwrite to go ahead "
    , "anyway)" ]

removeOldUnregisteredOutput :: Printer -> Buildsome -> FilePath -> IO ()
removeOldUnregisteredOutput printer buildsome path =
  case optOverwriteUnregisteredOutputs (bsOpts buildsome) of
  Opts.DontOverwriteUnregisteredOutputs -> E.throwIO $ UnregisteredOutputFileExists path
  Opts.OverwriteUnregisteredOutputs -> do
    printStrLn printer $ ColorText.render $
      "Overwriting " <> targetShow path <> " (due to --overwrite)"
    removeFileOrDirectory path

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
  , rcrInputs :: Map FilePath (FSHook.AccessType, Reason, Maybe Posix.FileStatus)
  , rcrOutputs :: Map FilePath Reason
  , rcrSlaveStats :: Slave.Stats
  }

recordInput ::
  IORef (Map FilePath (FSHook.AccessType, Reason, Maybe Posix.FileStatus)) ->
  Reason -> FSHook.Input -> IO ()
recordInput inputsRef reason (FSHook.Input accessType path) = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith merge path (accessType, reason, mstat)
  where
    merge (FSHook.AccessTypeFull, newReason, Just newStat)
          (oldAccessType, _oldReason, _oldMStat)
      | Posix.isDirectory newStat
      , oldAccessType < FSHook.AccessTypeFull =
      -- Special-case a read-directory that follows a stat/mode. The
      -- opendir will generate the targets inside, which will modify
      -- the directory's stats. This would be detected as "Third Party
      -- meddling", so for directories upgrading to full access type,
      -- we need to remember the new stat (which might miss third
      -- party meddling by others in this scenario in a (typically)
      -- very short time, so is not a big deal)
      (FSHook.AccessTypeFull, newReason, Just newStat)
    merge _ (oldAccessType, oldReason, oldMStat) =
     -- Keep the highest access type, and the oldest reason/mstat
     (max accessType oldAccessType, oldReason, oldMStat)

recordOutputs :: Buildsome -> IORef (Map FilePath Reason) -> Reason -> Set FilePath -> [FSHook.Output] -> IO ()
recordOutputs buildsome outputsRef accessDoc targetOutputsSet outputs =
  E.mask_ $ do
    atomicModifyIORef'_ outputsRef $ M.union $
      M.fromList [(path, accessDoc) | FSHook.Output path <- outputs]
    registerOutputs buildsome $ paths `S.intersection` targetOutputsSet
  where
    paths = S.fromList $ map FSHook.outputPath outputs

runCmd :: BuildTargetEnv -> Parallelism.Cell -> Target -> IO RunCmdResults
runCmd bte@BuildTargetEnv{..} parCell target = do
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  slaveStatsRef <- newIORef mempty
  let
    mappendSlaveStats stats = atomicModifyIORef_ slaveStatsRef (mappend stats)
    makeInputs accessDoc inputs outputs = do
      let allPaths = map FSHook.inputPath inputs ++ map FSHook.outputPath outputs
      parentSlaveStats <- buildParentDirectories bte parCell Implicit allPaths
      slaves <-
        fmap concat $ forM inputs $ \(FSHook.Input accessType path) ->
        mkSlavesForAccessType accessType bte { bteReason = accessDoc } Implicit path
      mappendSlaveStats . mappend parentSlaveStats =<< waitForSlaves btePrinter parCell bteBuildsome slaves
    outputIgnored (FSHook.Output path) = MagicFiles.outputIgnored path
    inputIgnored actualOutputs (FSHook.Input _ path) =
      MagicFiles.inputIgnored path ||
      path `M.member` actualOutputs ||
      path `S.member` targetOutputsSet
    fsAccessHandler isDelayed accessDoc inputs outputs = do
      actualOutputs <- readIORef outputsRef
      let filteredOutputs = filter (not . outputIgnored) outputs
          filteredInputs = filter (not . inputIgnored actualOutputs) inputs
      recordOutputs bteBuildsome outputsRef accessDoc targetOutputsSet filteredOutputs
      case isDelayed of
        FSHook.NotDelayed -> return ()
        FSHook.Delayed -> makeInputs accessDoc filteredInputs filteredOutputs
      mapM_ (recordInput inputsRef accessDoc) filteredInputs
  (time, stdOutputs) <-
    FSHook.timedRunCommand (bsFsHook bteBuildsome) (bsRootPath bteBuildsome)
    (shellCmdVerify target ["HOME", "PATH"])
    (ColorText.render (targetShow (targetOutputs target)))
    fsAccessHandler
  inputs <- readIORef inputsRef
  outputs <- readIORef outputsRef
  slaveStats <- readIORef slaveStatsRef
  return RunCmdResults
    { rcrStdOutputs = stdOutputs
    , rcrSelfTime = realToFrac time
    , rcrInputs = inputs `M.difference` outputs
    , rcrOutputs = outputs
    , rcrSlaveStats = slaveStats
    }
  where
    targetOutputsSet = S.fromList $ targetOutputs target

saveExecutionLog :: Buildsome -> Target -> RunCmdResults -> IO ()
saveExecutionLog buildsome target RunCmdResults{..} = do
  inputsDescs <- M.traverseWithKey inputAccess rcrInputs
  outputDescPairs <-
    forM (M.keys rcrOutputs) $ \outPath -> do
      fileDesc <- getFileDesc db outPath
      return (outPath, fileDesc)
  writeIRef (Db.executionLog target (bsDb buildsome)) Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = rcrStdOutputs
    , elSelfTime = rcrSelfTime
    }
  where
    db = bsDb buildsome
    inputAccess path (_, reason, _) | MagicFiles.allowedUnspecifiedOutput path = return (reason, Db.InputAccessIgnoredFile)
    inputAccess path (FSHook.AccessTypeFull, reason, mStat) = (,) reason . Db.InputAccessFull <$> fileDescOfMStat db path mStat
    inputAccess path (FSHook.AccessTypeModeOnly, reason, mStat) = (,) reason . Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat
    inputAccess path (FSHook.AccessTypeStat, reason, mStat) =
      (,) reason . Db.InputAccessStatOnly <$> fileStatDescOfMStat path mStat

buildTarget :: BuildTargetEnv -> Parallelism.Cell -> TargetRep -> Target -> IO Slave.Stats
buildTarget bte@BuildTargetEnv{..} parCell targetRep target = do
  mSlaveStats <- findApplyExecutionLog bte parCell targetRep target
  case mSlaveStats of
    Just slaveStats -> return slaveStats
    Nothing -> Print.targetWrap btePrinter bteReason target "BUILDING" $ do
      targetParentsStats <- buildParentDirectories bte parCell Explicit $ targetOutputs target

      registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb bteBuildsome
      mapM_ (removeOldOutput btePrinter bteBuildsome registeredOutputs) $ targetOutputs target

      slaves <-
        mkSlavesForPaths bte
          { bteReason = ColorText.render $ "Hint from " <> targetShow (targetOutputs target)
          } Explicit $ targetAllInputs target
      hintedStats <- waitForSlaves btePrinter parCell bteBuildsome slaves

      Print.cmd btePrinter target

      rcr@RunCmdResults{..} <- runCmd bte parCell target

      verifyTargetSpec bteBuildsome (M.keysSet rcrInputs) (M.keysSet rcrOutputs) target
      saveExecutionLog bteBuildsome target rcr

      printStrLn btePrinter $
        "Build (now) took " <> Color.timing (show rcrSelfTime <> " seconds")
      let selfStats = Slave.Stats $ M.singleton targetRep (Slave.BuiltNow, rcrSelfTime)
      return $ mconcat [selfStats, targetParentsStats, hintedStats, rcrSlaveStats]

registerDbList :: Ord a => (Db -> IORef (Set a)) -> Buildsome -> Set a -> IO ()
registerDbList mkIORef buildsome newItems =
  atomicModifyIORef'_ (mkIORef (bsDb buildsome)) (newItems <>)

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs = registerDbList Db.registeredOutputsRef

registerLeakedOutputs :: Buildsome -> Set FilePath -> IO ()
registerLeakedOutputs = registerDbList Db.leakedOutputsRef

deleteRemovedOutputs :: Buildsome -> IO ()
deleteRemovedOutputs buildsome = do
  toDelete <-
    atomicModifyIORef' (Db.registeredOutputsRef (bsDb buildsome)) $
    S.partition (isJust . BuildMaps.find (bsBuildMaps buildsome))
  forM_ (S.toList toDelete) $ \path -> do
    ColorText.putStrLn $ "Removing old output: " <> Color.path (show path)
    removeFileOrDirectoryOrNothing path

data TargetCommandFailed = TargetCommandFailed Target ExitCode (StdOutputs ByteString) deriving (Typeable)
instance E.Exception TargetCommandFailed
instance Show TargetCommandFailed where
  show (TargetCommandFailed target exitCode stdOutputs) =
    renderStr $ Color.error $ mconcat
    [ fromBytestring8 (Print.delimitMultiline cmd), " failed: ", show exitCode
    , fromMaybe "" $ Print.outputsStr " Outputs:" stdOutputs ]
    where
      cmd = targetCmds target

shellCmdVerify :: Target -> [String] -> Process.Env -> IO (StdOutputs ByteString)
shellCmdVerify target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand (BS8.unpack (targetCmds target))) inheritEnvs newEnvs
  let stdOutputs = StdOutputs stdout stderr
  case exitCode of
    ExitFailure {} -> E.throwIO $ TargetCommandFailed target exitCode stdOutputs
    _ -> return ()
  Print.targetStdOutputs target stdOutputs
  return stdOutputs

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

with :: FilePath -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
with makefilePath makefile opt@Opt{..} body =
  FSHook.with $ \fsHook ->
  Db.with (buildDbFilename makefilePath) $ \db -> do
    slaveMapByTargetRep <- newIORef M.empty
    parallelism <- Parallelism.new $ fromMaybe 1 optParallelism
    freshPrinterIds <- Fresh.new 1
    buildId <- BuildId.new
    rootPath <- FilePath.canonicalizePath $ FilePath.takeDirectory makefilePath
    let
      buildsome =
        Buildsome
        { bsOpts = opt
        , bsMakefile = makefile
        , bsBuildId = buildId
        , bsRootPath = rootPath
        , bsBuildMaps = BuildMaps.make makefile
        , bsDb = db
        , bsFsHook = fsHook
        , bsSlaveByTargetRep = slaveMapByTargetRep
        , bsParallelism = parallelism
        , bsFreshPrinterIds = freshPrinterIds
        }
    deleteRemovedOutputs buildsome
    body buildsome
      -- We must not leak running slaves as we're not allowed to
      -- access fsHook, db, etc after leaving here:
      `E.onException` putStrLn "Shutting down"
      `finally` cancelAllSlaves buildsome
      -- Must update gitIgnore after all the slaves finished updating
      -- the registered output lists:
      `finally` maybeUpdateGitIgnore buildsome
    where
      maybeUpdateGitIgnore buildsome =
        case optUpdateGitIgnore of
        Opts.UpdateGitIgnore -> updateGitIgnore buildsome makefilePath
        Opts.DontUpdateGitIgnore -> return ()

clean :: Buildsome -> IO ()
clean buildsome = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  Clean.Result _totalSize totalSpace count <-
    mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
  writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
  writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
  ColorText.putStrLn $ mconcat
    [ Color.success "Clean Successful", ": Cleaned "
    , show count, " files freeing an estimated "
    , showBytes (fromIntegral totalSpace)
    ]
