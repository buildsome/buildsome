{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, partition, intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList, isJust)
import Data.Monoid
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Db (Db, IRef(..), Reason)
import Lib.AccessType (AccessType(..))
import Lib.AnnotatedException (annotateException)
import Lib.Async (wrapAsync)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing)
import Lib.FSHook (FSHook, Handlers(..))
import Lib.FileDesc (fileDescOfMStat, getFileDesc, fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath ((</>), canonicalizePath, splitFileName)
import Lib.IORef (atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), TargetType(..), Target)
import Lib.Printer (Printer)
import Lib.ShowBytes (showBytes)
import Lib.Sigint (installSigintHandler)
import Lib.StdOutputs (StdOutputs(..), printStdouts)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..), OverwriteUnregisteredOutputs(..))
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (<.>), makeRelative)
import System.Posix.Files (FileStatus, fileExist)
import System.Process (CmdSpec(..))
import qualified Clean
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Db
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.FSHook as FSHook
import qualified Lib.Makefile as Makefile
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified System.Directory as Dir
import qualified System.IO as IO

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]

newtype Slave = Slave { slaveExecution :: Async () }

data Buildsome = Buildsome
  { bsSlaveByRepPath :: IORef (Map TargetRep (MVar Slave))
  , bsDeleteUnspecifiedOutput :: DeleteUnspecifiedOutputs
  , bsOverwriteUnregisteredOutputs :: OverwriteUnregisteredOutputs
  , bsBuildMaps :: BuildMaps
  , bsRestrictedParallelism :: MSem Int
  , bsDb :: Db
  , bsMakefile :: Makefile
  , bsRootPath :: FilePath
  , bsFsHook :: FSHook
  , bsNextPrinterId :: IORef Int
  }

nextPrinter :: Buildsome -> IO Printer
nextPrinter buildsome = do
  printerId <- atomicModifyIORef (bsNextPrinterId buildsome) $ \oldId -> (oldId+1, oldId+1)
  Printer.new printerId

slaveWait :: Slave -> IO ()
slaveWait = wait . slaveExecution

-- | Opposite of MSem.with
localSemSignal :: MSem Int -> IO a -> IO a
localSemSignal sem = E.bracket_ (MSem.signal sem) (MSem.wait sem)

withReleasedParallelism :: Buildsome -> IO a -> IO a
withReleasedParallelism = localSemSignal . bsRestrictedParallelism

withAllocatedParallelism :: Buildsome -> IO a -> IO a
withAllocatedParallelism = MSem.with . bsRestrictedParallelism

allowedUnspecifiedOutput :: FilePath -> Bool
allowedUnspecifiedOutput = (".pyc" `isSuffixOf`)

recordInput ::
  IORef (Map FilePath (AccessType, Reason, Maybe FileStatus)) ->
  AccessType -> Reason -> FilePath -> IO ()
recordInput inputsRef accessType reason path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith merge path (accessType, reason, mstat)
  where
    merge _ (oldAccessType, oldReason, oldMStat) =
     -- Keep the highest access type, and the oldest reason/mstat
     (max accessType oldAccessType, oldReason, oldMStat)

inputIgnored :: FilePath -> Bool
inputIgnored path = any (`isPrefixOf` path) ["/dev", "/proc", "/sys"]

verifyCancelled :: Async a -> IO (Either E.SomeException a)
verifyCancelled a = do
  cancel a
  waitCatch a

cancelAllSlaves :: Buildsome -> IO ()
cancelAllSlaves bs = go 0
  where
    go alreadyCancelled = do
      curSlaveMap <- readIORef $ bsSlaveByRepPath bs
      slaves <- mapM readMVar $ M.elems curSlaveMap
      let count = length slaves
      if alreadyCancelled >= count
        then return ()
        else do
          mapM_ (verifyCancelled . slaveExecution) slaves
          -- Make sure to cancel any potential new slaves that were
          -- created during cancellation
          go count

withBuildsome :: FilePath -> FSHook -> Db -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
withBuildsome makefilePath fsHook db makefile opt body = do
  slaveMapByRepPath <- newIORef M.empty
  semaphore <- MSem.new parallelism
  nextPrinterId <- newIORef 0
  let
    buildsome =
      Buildsome
      { bsSlaveByRepPath = slaveMapByRepPath
      , bsBuildMaps = BuildMaps.make makefile
      , bsDeleteUnspecifiedOutput = deleteUnspecifiedOutput
      , bsOverwriteUnregisteredOutputs = overwriteUnregisteredOutputs
      , bsRestrictedParallelism = semaphore
      , bsDb = db
      , bsMakefile = makefile
      , bsRootPath = takeDirectory makefilePath
      , bsFsHook = fsHook
      , bsNextPrinterId = nextPrinterId
      }
  (body buildsome
    `E.finally` maybeUpdateGitIgnore buildsome)
    -- We must not leak running slaves as we're not allowed to
    -- access fsHook, db, etc after leaving here:
    `E.finally` cancelAllSlaves buildsome
  where
    maybeUpdateGitIgnore buildsome
      | writeGitIgnore = updateGitIgnore buildsome makefilePath
      | otherwise = return ()
    parallelism = fromMaybe 1 mParallelism
    Opt _targets _mMakefile mParallelism
        writeGitIgnore deleteUnspecifiedOutput
        overwriteUnregisteredOutputs = opt

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  writeFile gitIgnorePath $ unlines $
    map (makeRelative dir) $
    extraIgnored ++ S.toList (outputs <> leaked)

need :: Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO ()
need buildsome explicitness reason parents paths = do
  slaves <- concat <$> mapM (makeSlaves buildsome explicitness reason parents) paths
  mapM_ slaveWait slaves

want :: Buildsome -> Reason -> [FilePath] -> IO ()
want buildsome reason = need buildsome Explicit reason []

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- fileExist path
  unless doesExist $ E.throwIO err

data MissingRule = MissingRule FilePath Reason deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule path reason) = concat ["ERROR: No rule to build ", show path, " (", reason, ")"]

data TargetNotCreated = TargetNotCreated FilePath deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated path) = concat
    [ show path
    , " explicitly demanded but was not created by its target rule"
    ]

makeDirectSlave :: Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO (Maybe Slave)
makeDirectSlave buildsome explicitness reason parents path =
  case BuildMaps.find (bsBuildMaps buildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $ assertExists path $ MissingRule path reason
    return Nothing
  Just tgt -> do
    slave <- getSlaveForTarget buildsome reason parents tgt
    Just <$> case explicitness of
      Implicit -> return slave
      Explicit -> verifyFileGetsCreated slave
  where
    verifyFileGetsCreated slave
      | path `elem` makefilePhonies (bsMakefile buildsome) = return slave
      | otherwise = do
      wrappedExecution <-
        wrapAsync (slaveExecution slave) $ \() ->
        assertExists path $ TargetNotCreated path
      return slave { slaveExecution = wrappedExecution }

makeChildSlaves :: Buildsome -> Reason -> Parents -> FilePath -> IO [Slave]
makeChildSlaves buildsome reason parents path
  | not (null childPatterns) =
    fail "UNSUPPORTED: Read directory on directory with patterns"
  | otherwise =
    traverse (getSlaveForTarget buildsome reason parents)
    childTargets
  where
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps buildsome) path

makeSlavesForAccessType ::
  AccessType -> Buildsome -> Explicitness -> Reason ->
  Parents -> FilePath -> IO [Slave]
makeSlavesForAccessType accessType buildsome explicitness reason parents path =
  case accessType of
  AccessTypeFull ->
    makeSlaves buildsome explicitness reason parents path
  AccessTypeModeOnly ->
    maybeToList <$> makeDirectSlave buildsome explicitness reason parents path

makeSlaves :: Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO [Slave]
makeSlaves buildsome explicitness reason parents path = do
  mSlave <- makeDirectSlave buildsome explicitness reason parents path
  childs <- makeChildSlaves buildsome (reason ++ "(Container directory)") parents path
  return $ maybeToList mSlave ++ childs

-- e.g: .pyc files
handleLegalUnspecifiedOutputs :: Printer -> Buildsome -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs printer buildsome target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ Printer.putStrLn printer $ concat
    [ "WARNING: Leaked unspecified outputs: "
    , show paths, " from target for: ", show (targetOutputs target)
    , ", ", actionDesc
    ]
  action
  where
    (actionDesc, action) =
      case bsDeleteUnspecifiedOutput buildsome of
      DeleteUnspecifiedOutputs -> ("deleting", mapM_ Dir.removeFile paths)
      DontDeleteUnspecifiedOutputs -> ("keeping", registerLeakedOutputs buildsome (S.fromList paths))

data IllegalUnspecifiedOutputs = IllegalUnspecifiedOutputs Target [FilePath] deriving (Typeable)
instance E.Exception IllegalUnspecifiedOutputs
instance Show IllegalUnspecifiedOutputs where
  show (IllegalUnspecifiedOutputs target illegalOutputs) = concat
    [ "Target: ", show (targetOutputs target)
    , " wrote to unspecified output files: ", show illegalOutputs ]

-- Verify output of whole of slave/execution log
verifyTargetOutputs :: Printer -> Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs printer buildsome outputs target = do

  let (unspecifiedOutputs, illegalOutputs) = partition allowedUnspecifiedOutput allUnspecified

  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs printer buildsome target =<<
    filterM fileExist unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM fileExist illegalOutputs
  unless (null existingIllegalOutputs) $ do
    Printer.putStrLn printer $ "Illegal output files created: " ++ show existingIllegalOutputs
    mapM_ removeFileOrDirectory existingIllegalOutputs
    E.throwIO $ IllegalUnspecifiedOutputs target existingIllegalOutputs
  unless (S.null unusedOutputs) $
    Printer.putStrLn printer $ "WARNING: Over-specified outputs: " ++ show (S.toList unusedOutputs)
  where
    phonies = S.fromList $ makefilePhonies $ bsMakefile buildsome
    unusedOutputs = (specified `S.difference` outputs) `S.difference` phonies
    allUnspecified = S.toList $ outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

saveExecutionLog ::
  Buildsome -> Target ->
  Map FilePath (AccessType, Reason, Maybe FileStatus) ->
  Set FilePath -> StdOutputs -> IO ()
saveExecutionLog buildsome target inputs outputs stdOutputs = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM (S.toList outputs) $ \outPath -> do
      fileDesc <- getFileDesc outPath
      return (outPath, fileDesc)
  writeIRef (Db.executionLog target (bsDb buildsome)) $
    Db.ExecutionLog inputsDescs (M.fromList outputDescPairs) stdOutputs
  where
    inputAccess path (AccessTypeFull, reason, mStat) = (,) reason . Db.InputAccessFull <$> fileDescOfMStat path mStat
    inputAccess path (AccessTypeModeOnly, reason, mStat) = (,) reason . Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

targetPrintWrap :: Printer -> Target -> String -> Reason -> IO a -> IO a
targetPrintWrap printer target str reason body =
  Printer.printWrap printer
    (show (targetOutputs target)) $ do
    Printer.putStrLn printer $ concat [str, " (", reason, ")"]
    unless (null cmd) $ Printer.putStrLn printer cmd
    body
  where
    cmd = targetCmds target

-- Already verified that the execution log is a match
applyExecutionLog ::
  Printer -> Buildsome -> Target -> Reason -> Set FilePath -> StdOutputs -> IO ()
applyExecutionLog printer buildsome target reason outputs stdOutputs =
  targetPrintWrap printer target "REPLAY" reason $ do
    printStdouts (show (targetOutputs target)) stdOutputs
    verifyTargetOutputs printer buildsome outputs target

tryApplyExecutionLog ::
  Printer -> Buildsome -> Target -> Reason -> Parents -> Db.ExecutionLog ->
  IO (Either (String, FilePath) ())
tryApplyExecutionLog printer buildsome target reason parents executionLog = do
  waitForInputs
  runEitherT $ do
    forM_ (M.toList inputsDescs) $ \(filePath, oldInputAccess) ->
      case snd oldInputAccess of
        Db.InputAccessFull oldDesc ->         compareToNewDesc "input"       getFileDesc     (filePath, oldDesc)
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc (filePath, oldModeDesc)
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    mapM_ (compareToNewDesc "output" getFileDesc) $ M.toList outputsDescs

    liftIO $
      applyExecutionLog printer buildsome target reason
      (M.keysSet outputsDescs) stdOutputs
  where
    Db.ExecutionLog inputsDescs outputsDescs stdOutputs = executionLog
    compareToNewDesc str getNewDesc (filePath, oldDesc) = do
      newDesc <- liftIO $ getNewDesc filePath
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation
    inputAccessToType Db.InputAccessModeOnly {} = AccessTypeModeOnly
    inputAccessToType Db.InputAccessFull {} = AccessTypeFull
    waitForInputs = do
      -- TODO: This is good for parallelism, but bad if the set of
      -- inputs changed, as it may build stuff that's no longer
      -- required:
      let hinted = S.fromList $ targetAllInputs target
      speculativeSlaves <-
        fmap concat $ forM (M.toList inputsDescs) $ \(inputPath, (depReason, inputAccess)) ->
        if inputPath `S.member` hinted
        then return []
        else makeSlavesForAccessType (inputAccessToType inputAccess) buildsome Implicit depReason parents inputPath

      let hintReason = "Hint from " ++ show (take 1 (targetOutputs target))
      hintedSlaves <- concat <$> mapM (makeSlaves buildsome Explicit hintReason parents) (targetAllInputs target)

      mapM_ slaveWait (speculativeSlaves ++ hintedSlaves)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: Printer -> Buildsome -> Target -> Reason -> Parents -> IO Bool
findApplyExecutionLog printer buildsome target reason parents = do
  mExecutionLog <- readIRef $ Db.executionLog target $ bsDb buildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just executionLog -> do
      res <- tryApplyExecutionLog printer buildsome target reason parents executionLog
      case res of
        Left (str, filePath) -> do
          Printer.putStrLn printer $ concat
            ["Execution log of ", show (targetOutputs target), " did not match because ", str, ": ", show filePath, " changed"]
          return False
        Right () -> return True

showParents :: Parents -> String
showParents = concatMap showParent
  where
    showParent (targetRep, reason) = concat ["\n-> ", show targetRep, " (", reason, ")"]

data TargetDependencyLoop = TargetDependencyLoop Parents deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop parents) = "Target loop: " ++ showParents parents

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: Buildsome -> Reason -> Parents -> (TargetRep, Target) -> IO Slave
getSlaveForTarget buildsome reason parents (targetRep, target)
  | any ((== targetRep) . fst) parents = E.throwIO $ TargetDependencyLoop newParents
  | otherwise = do
    newSlaveMVar <- newEmptyMVar
    E.mask $ \restoreMask -> join $
      atomicModifyIORef (bsSlaveByRepPath buildsome) $
      \oldSlaveMap ->
      -- TODO: Use a faster method to lookup&insert at the same time
      case M.lookup targetRep oldSlaveMap of
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
      Nothing ->
        ( M.insert targetRep newSlaveMVar oldSlaveMap
        , mkSlave newSlaveMVar $ restoreMask $ do
            printer <- nextPrinter buildsome
            success <- findApplyExecutionLog printer buildsome target reason parents
            unless success $ buildTarget printer buildsome target reason parents
        )
    where
      annotate = annotateException $ "build failure of " ++ show (targetOutputs target) ++ ":\n"
      newParents = (targetRep, reason) : parents
      mkSlave mvar action = do
        slave <- fmap Slave . async $ annotate action
        putMVar mvar slave >> return slave

data UnregisteredOutputFileExists = UnregisteredOutputFileExists FilePath deriving (Typeable)
instance E.Exception UnregisteredOutputFileExists
instance Show UnregisteredOutputFileExists where
  show (UnregisteredOutputFileExists path) = concat
    [ show path, " specified as output but exists as a file that "
    , "was not created by buildsome (use --overwrite to go ahead "
    , "anyway)" ]

removeOldUnregisteredOutput :: Printer -> Buildsome -> FilePath -> IO ()
removeOldUnregisteredOutput printer buildsome path =
  case bsOverwriteUnregisteredOutputs buildsome of
  DontOverwriteUnregisteredOutputs -> E.throwIO $ UnregisteredOutputFileExists path
  OverwriteUnregisteredOutputs -> do
    Printer.putStrLn printer $ "Overwriting " ++ show path ++ " (due to --overwrite)"
    removeFileOrDirectory path

removeOldOutput :: Printer -> Buildsome -> Set FilePath -> FilePath -> IO ()
removeOldOutput printer buildsome registeredOutputs path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> return () -- Nothing to do
    Just _
      | path `S.member` registeredOutputs -> removeFileOrDirectory path
      | otherwise -> removeOldUnregisteredOutput printer buildsome path

buildTarget :: Printer -> Buildsome -> Target -> Reason -> Parents -> IO ()
buildTarget printer buildsome target reason parents =
  targetPrintWrap printer target "BUILDING" reason $ do
    -- TODO: Register each created subdirectory as an output?
    mapM_ (Dir.createDirectoryIfMissing True . takeDirectory) $ targetOutputs target

    registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
    mapM_ (removeOldOutput printer buildsome registeredOutputs) $ targetOutputs target

    need buildsome Explicit
      ("Hint from " ++ show (take 1 (targetOutputs target))) parents
      (targetAllInputs target)
    inputsRef <- newIORef M.empty
    outputsRef <- newIORef S.empty

    stdOutputs <-
      withAllocatedParallelism buildsome $
      Printer.printWrap printer ("runCmd" ++ show (targetOutputs target))
      (runCmd buildsome target parents inputsRef outputsRef)
      `E.finally` do
        outputs <- readIORef outputsRef
        registerOutputs buildsome $ S.intersection outputs $ S.fromList $ targetOutputs target

    outputs <- readIORef outputsRef
    verifyTargetOutputs printer buildsome outputs target

    inputs <- readIORef inputsRef
    saveExecutionLog buildsome target inputs outputs stdOutputs

registerDbList :: Ord a => (Db -> IORef (Set a)) -> Buildsome -> Set a -> IO ()
registerDbList mkIORef buildsome newItems =
  atomicModifyIORef'_ (mkIORef (bsDb buildsome)) $ (newItems <>)

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
    putStrLn $ "Removing old output: " ++ show path
    removeFileOrDirectoryOrNothing path

data CommandFailed = CommandFailed String ExitCode deriving (Typeable)
instance E.Exception CommandFailed
instance Show CommandFailed where
  show (CommandFailed cmd exitCode)
    | '\n' `elem` cmd = "\"\"\"" ++ cmd ++ "\"\"\"" ++ suffix
    | otherwise = show cmd ++ suffix
    where
      suffix = " failed: " ++ show exitCode

shellCmdVerify :: Target -> [String] -> Process.Env -> IO StdOutputs
shellCmdVerify target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand cmd) inheritEnvs newEnvs
  let stdouts = StdOutputs stdout stderr
  printStdouts (show (targetOutputs target)) stdouts
  case exitCode of
    ExitFailure {} -> E.throwIO $ CommandFailed cmd exitCode
    _ -> return stdouts
  where
    cmd = targetCmds target

runCmd ::
  Buildsome -> Target -> Parents ->
  -- TODO: Clean this arg list up
  IORef (Map FilePath (AccessType, Reason, Maybe FileStatus)) ->
  IORef (Set FilePath) -> IO StdOutputs
runCmd buildsome target parents inputsRef outputsRef = do
  rootPath <- Dir.canonicalizePath (bsRootPath buildsome)
  FSHook.runCommand (bsFsHook buildsome) rootPath
    (shellCmdVerify target ["HOME", "PATH"])
    (show (targetOutputs target))
    Handlers {..}
  where
    handleInputCommon accessType actDesc path useInput
      | inputIgnored path = return ()
      | otherwise = do
        actualOutputs <- readIORef outputsRef
        -- There's no problem for a target to read its own outputs freely:
        unless (path `S.member` actualOutputs) $ do
          () <- useInput
          unless (path `elem` targetOutputs target || allowedUnspecifiedOutput path) $
            recordInput inputsRef accessType actDesc path
    handleInput accessType actDesc path =
      handleInputCommon accessType actDesc path $ return ()
    handleDelayedInput accessType actDesc path =
      handleInputCommon accessType actDesc path $ do
        slaves <- makeSlavesForAccessType accessType buildsome Implicit actDesc parents path
        -- Temporarily paused, so we can temporarily release parallelism
        -- semaphore
        unless (null slaves) $
          withReleasedParallelism buildsome $
          mapM_ slaveWait slaves
    handleOutput _actDesc path =
      atomicModifyIORef'_ outputsRef $ S.insert path

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

standardMakeFilename :: String
standardMakeFilename = "Makefile"

data MakefileScanFailed = MakefileScanFailed deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show MakefileScanFailed = "ERROR: Cannot find a file named " ++ show standardMakeFilename ++ " in this directory or any of its parents"

findMakefile :: IO FilePath
findMakefile = do
  cwd <- Dir.getCurrentDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate takeDirectory cwd
    candidates = map (</> standardMakeFilename) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runEitherT $ mapM_ check candidates
  case res of
    Left found -> Dir.makeRelativeToCurrentDirectory found
    Right () -> E.throwIO MakefileScanFailed
  where
    check path = do
      exists <- liftIO $ fileExist path
      when exists $ left path

specifiedMakefile :: FilePath -> IO FilePath
specifiedMakefile path = do
  _ <- Dir.canonicalizePath path -- Verifies it exists
  d <- Dir.doesDirectoryExist path
  return $ if d then path </> "Makefile" else path

data Requested = RequestedClean | RequestedTargets [FilePath] Reason

data BadCommandLine = BadCommandLine String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine msg) = "Invalid command line options: " ++ msg

getRequestedTargets :: [String] -> IO Requested
getRequestedTargets ["clean"] = return RequestedClean
getRequestedTargets [] = return $ RequestedTargets ["default"] "implicit 'default' target"
getRequestedTargets ts
  | "clean" `elem` ts = E.throwIO $ BadCommandLine "Clean must be requested exclusively"
  | otherwise = return $ RequestedTargets ts "explicit request from cmdline"

setBuffering :: IO ()
setBuffering = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

main :: IO ()
main = do
  installSigintHandler
  setBuffering
  FSHook.with $ \fsHook -> do
    opt <- getOpt
    makefilePath <- maybe findMakefile specifiedMakefile $ optMakefilePath opt
    putStrLn $ "Using makefile: " ++ show makefilePath
    let (cwd, file) = splitFileName makefilePath
    origCwd <- Dir.getCurrentDirectory
    Dir.setCurrentDirectory cwd
    origMakefile <- Makefile.parse file
    makefile <- Makefile.onMakefilePaths canonicalizePath origMakefile
    Db.with (buildDbFilename file) $ \db ->
      withBuildsome file fsHook db makefile opt $
        \buildsome -> do
        deleteRemovedOutputs buildsome
        let
          inOrigCwd =
            case optMakefilePath opt of
            -- If we found the makefile by scanning upwards, prepend
            -- original cwd to avoid losing it:
            Nothing -> mapM (canonicalizePath . (origCwd </>))
            -- Otherwise: there's no useful original cwd:
            Just _ -> return

        requested <- getRequestedTargets $ optRequestedTargets opt

        case requested of
          RequestedTargets requestedTargets reason -> do
            requestedTargetPaths <- inOrigCwd requestedTargets
            putStrLn $ "Building: " ++ intercalate ", " (map show requestedTargetPaths)
            want buildsome reason requestedTargetPaths
            putStrLn "Build Successful!"
          RequestedClean -> do
            outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
            leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
            Clean.Result _totalSize totalSpace count <-
              mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
            writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
            writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
            putStrLn $ concat
              [ "Clean Successful: Cleaned "
              , show count, " files freeing an estimated "
              , showBytes (fromIntegral totalSpace)
              ]
