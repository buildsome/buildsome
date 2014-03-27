{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (partition, intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList, isJust)
import Data.Monoid
import Data.Set (Set)
import Data.Time (DiffTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Db (Db, IRef(..), Reason)
import FileDescCache (getFileDesc, fileDescOfMStat)
import Lib.AccessType (AccessType(..))
import Lib.AnnotatedException (annotateException)
import Lib.Async (wrapAsync)
import Lib.BuildId (BuildId)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing, createDirectories)
import Lib.Exception (finally)
import Lib.FSHook (FSHook, Handlers(..))
import Lib.FileDesc (fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath (FilePath, (</>), (<.>), canonicalizePath, canonicalizePathAsRelative, splitFileName, takeDirectory, makeRelative, makeRelativeToCurrentDirectory)
import Lib.IORef (atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), TargetType(..), Target)
import Lib.PoolAlloc (PoolAlloc)
import Lib.Printer (Printer)
import Lib.ShowBytes (showBytes)
import Lib.Sigint (installSigintHandler)
import Lib.StdOutputs (StdOutputs(..), printStdouts)
import Lib.TimeIt (timeIt)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..), OverwriteUnregisteredOutputs(..))
import Prelude hiding (FilePath)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..))
import qualified Clean
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Db
import qualified Lib.BuildId as BuildId
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.FSHook as FSHook
import qualified Lib.Makefile as Makefile
import qualified Lib.PoolAlloc as PoolAlloc
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Timeout as Timeout
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]

data Slave = Slave
  { slaveExecution :: Async ()
  , slavePrinterId :: Printer.Id
  , slaveOutputPaths :: [FilePath]
  }

slaveStr :: Slave -> String
slaveStr slave = Printer.idStr (slavePrinterId slave) ++ ": " ++ show (slaveOutputPaths slave)

type ParId = Int
type ParCell = IORef ParId

data Buildsome = Buildsome
  { bsSlaveByRepPath :: IORef (Map TargetRep (MVar Slave))
  , bsDeleteUnspecifiedOutput :: DeleteUnspecifiedOutputs
  , bsOverwriteUnregisteredOutputs :: OverwriteUnregisteredOutputs
  , bsBuildMaps :: BuildMaps
  , bsParallelismPool :: PoolAlloc ParId
  , bsDb :: Db
  , bsMakefile :: Makefile
  , bsRootPath :: FilePath
  , bsFsHook :: FSHook
  , bsNextPrinterId :: IORef Printer.Id
  , bsBuildId :: BuildId
  }

nextPrinterId :: Buildsome -> IO Printer.Id
nextPrinterId buildsome = atomicModifyIORef (bsNextPrinterId buildsome) $ \oldId -> (oldId+1, oldId+1)

slaveWait :: Printer -> Slave -> IO ()
slaveWait _printer slave =
  -- Keep this around so we can enable logging about slave waits
  -- easily:
  -- Printer.printWrap _printer ("Waiting for " ++ slaveStr slave) $
  wait $ slaveExecution slave

-- | Release the currently held item, run given action, then regain
-- new item instead
localReleasePool :: PoolAlloc a -> IORef a -> IO b -> IO b
localReleasePool pool ref = E.bracket_ release alloc
  where
    markError = writeIORef ref $ error "Attempt to read released resource"
    release = do
      PoolAlloc.release pool =<< readIORef ref
      markError
    alloc = writeIORef ref =<< PoolAlloc.alloc pool

withReleasedParallelism :: ParCell -> Buildsome -> IO a -> IO a
withReleasedParallelism parCell bs = localReleasePool (bsParallelismPool bs) parCell

allowedUnspecifiedOutput :: FilePath -> Bool
allowedUnspecifiedOutput = (".pyc" `BS8.isSuffixOf`)

recordInput ::
  IORef (Map FilePath (AccessType, Reason, Maybe Posix.FileStatus)) ->
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

specialFile :: FilePath -> Bool
specialFile path = any (`BS8.isPrefixOf` path) ["/dev", "/proc", "/sys"]

inputIgnored :: FilePath -> Bool
inputIgnored = specialFile

outputIgnored :: FilePath -> Bool
outputIgnored = specialFile

verifyCancelled :: Async a -> IO (Either E.SomeException a)
verifyCancelled a = do
  cancel a
  waitCatch a

cancelAllSlaves :: Buildsome -> IO ()
cancelAllSlaves bs = go 0
  where
    timeoutVerifyCancelled slave =
      Timeout.warning (Timeout.seconds 2)
      (unwords ["Slave", slaveStr slave, "did not cancel in 2 seconds!"]) $
      verifyCancelled $ slaveExecution slave
    go alreadyCancelled = do
      curSlaveMap <- readIORef $ bsSlaveByRepPath bs
      slaves <-
        forM (M.toList curSlaveMap) $
        \(repPath, mvar) ->
        Timeout.warning (Timeout.millis 100)
        ("Slave MVar for " ++ show repPath ++ " not populated in 100 millis!") $
        readMVar mvar
      let count = length slaves
      if alreadyCancelled >= count
        then return ()
        else do
          mapM_ timeoutVerifyCancelled slaves
          -- Make sure to cancel any potential new slaves that were
          -- created during cancellation
          go count

withBuildsome :: FilePath -> FSHook -> Db -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
withBuildsome makefilePath fsHook db makefile opt body = do
  slaveMapByRepPath <- newIORef M.empty
  pool <- PoolAlloc.new [1..parallelism]
  printerIdRef <- newIORef 0
  buildId <- BuildId.new
  let
    buildsome =
      Buildsome
      { bsSlaveByRepPath = slaveMapByRepPath
      , bsBuildMaps = BuildMaps.make makefile
      , bsDeleteUnspecifiedOutput = deleteUnspecifiedOutput
      , bsOverwriteUnregisteredOutputs = overwriteUnregisteredOutputs
      , bsParallelismPool = pool
      , bsDb = db
      , bsMakefile = makefile
      , bsRootPath = takeDirectory makefilePath
      , bsFsHook = fsHook
      , bsNextPrinterId = printerIdRef
      , bsBuildId = buildId
      }
  body buildsome
    -- We must not leak running slaves as we're not allowed to
    -- access fsHook, db, etc after leaving here:
    `finally` cancelAllSlaves buildsome
    -- Must update gitIgnore after all the slaves finished updating
    -- the registered output lists:
    `finally` maybeUpdateGitIgnore buildsome
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
  putStrLn "Updating .gitignore"
  BS8.writeFile (BS8.unpack gitIgnorePath) $ BS8.unlines $
    map (("/" <>) . makeRelative dir) $
    extraIgnored ++ S.toList (outputs <> leaked)

mkSlavesForPaths :: Printer -> Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO [Slave]
mkSlavesForPaths printer buildsome explicitness reason parents paths =
  concat <$> mapM (makeSlaves printer buildsome explicitness reason parents) paths

need :: Printer -> Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO ()
need printer buildsome explicitness reason parents paths =
  mapM_ (slaveWait printer) =<< mkSlavesForPaths printer buildsome explicitness reason parents paths

want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO ()
want printer buildsome reason = need printer buildsome Explicit reason []

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- Posix.fileExist path
  unless doesExist $ E.throwIO err

data MissingRule = MissingRule FilePath Reason deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule path reason) = concat ["ERROR: No rule to build ", show path, " (", BS8.unpack reason, ")"]

data TargetNotCreated = TargetNotCreated FilePath deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated path) = concat
    [ show path
    , " explicitly demanded but was not created by its target rule"
    ]

makeDirectSlave :: Printer -> Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO (Maybe Slave)
makeDirectSlave printer buildsome explicitness reason parents path =
  case BuildMaps.find (bsBuildMaps buildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $ assertExists path $ MissingRule path reason
    return Nothing
  Just tgt -> do
    slave <- getSlaveForTarget printer buildsome reason parents tgt
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

makeChildSlaves :: Printer -> Buildsome -> Reason -> Parents -> FilePath -> IO [Slave]
makeChildSlaves printer buildsome reason parents path
  | not (null childPatterns) =
    fail "UNSUPPORTED: Read directory on directory with patterns"
  | otherwise =
    traverse (getSlaveForTarget printer buildsome reason parents)
    childTargets
  where
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps buildsome) path

makeSlavesForAccessType ::
  AccessType -> Printer -> Buildsome -> Explicitness -> Reason ->
  Parents -> FilePath -> IO [Slave]
makeSlavesForAccessType accessType printer buildsome explicitness reason parents path =
  case accessType of
  AccessTypeFull ->
    makeSlaves printer buildsome explicitness reason parents path
  AccessTypeModeOnly ->
    maybeToList <$> makeDirectSlave printer buildsome explicitness reason parents path

makeSlaves :: Printer -> Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO [Slave]
makeSlaves printer buildsome explicitness reason parents path = do
  mSlave <- makeDirectSlave printer buildsome explicitness reason parents path
  childs <- makeChildSlaves printer buildsome (reason <> "(Container directory)") parents path
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
      DeleteUnspecifiedOutputs -> ("deleting", mapM_ removeFileOrDirectoryOrNothing paths)
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
    filterM Posix.fileExist unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM Posix.fileExist illegalOutputs
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
  Map FilePath (AccessType, Reason, Maybe Posix.FileStatus) ->
  Set FilePath -> StdOutputs -> DiffTime -> IO ()
saveExecutionLog buildsome target inputs outputs stdOutputs selfTime = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM (S.toList outputs) $ \outPath -> do
      fileDesc <- getFileDesc db outPath
      return (outPath, fileDesc)
  writeIRef (Db.executionLog target (bsDb buildsome)) $ Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = stdOutputs
    , elSelfTime = selfTime
    }
  where
    db = bsDb buildsome
    inputAccess path (AccessTypeFull, reason, mStat) = (,) reason . Db.InputAccessFull <$> fileDescOfMStat db path mStat
    inputAccess path (AccessTypeModeOnly, reason, mStat) = (,) reason . Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

targetPrintWrap :: Printer -> Target -> ByteString -> Reason -> IO a -> IO a
targetPrintWrap printer target str reason body =
  Printer.printWrap printer
    (show (targetOutputs target)) $ do
    Printer.bsPutStrLn printer $ BS8.concat [str, " (", reason, ")"]
    unless (BS8.null cmd) $ Printer.bsPutStrLn printer cmd
    body
  where
    cmd = targetCmds target

-- Already verified that the execution log is a match
applyExecutionLog ::
  Printer -> Buildsome -> Target -> Reason -> Set FilePath -> StdOutputs -> DiffTime -> IO ()
applyExecutionLog printer buildsome target reason outputs stdOutputs selfTime =
  targetPrintWrap printer target "REPLAY" reason $ do
    printStdouts (show (targetOutputs target)) stdOutputs
    verifyTargetOutputs printer buildsome outputs target
    Printer.putStrLn printer $ "Build (originally) took " ++ show selfTime ++ " seconds"

waitForSlaves :: Printer -> ParCell -> Buildsome -> [Slave] -> IO ()
waitForSlaves printer parCell buildsome slaves =
  withReleasedParallelism parCell buildsome $
  mapM_ (slaveWait printer) slaves

tryApplyExecutionLog ::
  Printer -> ParCell -> Buildsome ->
  Target -> Reason -> Parents -> Db.ExecutionLog ->
  IO (Either (String, FilePath) ())
tryApplyExecutionLog printer parCell buildsome target reason parents Db.ExecutionLog {..} = do
  waitForInputs
  runEitherT $ do
    forM_ (M.toList elInputsDescs) $ \(filePath, oldInputAccess) ->
      case snd oldInputAccess of
        Db.InputAccessFull oldDesc ->         compareToNewDesc "input"       (getFileDesc db) (filePath, oldDesc)
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc  (filePath, oldModeDesc)
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    mapM_ (compareToNewDesc "output" (getFileDesc db)) $ M.toList elOutputsDescs

    liftIO $
      applyExecutionLog printer buildsome target reason
      (M.keysSet elOutputsDescs) elStdoutputs elSelfTime
  where
    db = bsDb buildsome
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
        fmap concat $ forM (M.toList elInputsDescs) $ \(inputPath, (depReason, inputAccess)) ->
        if inputPath `S.member` hinted
        then return []
        else makeSlavesForAccessType (inputAccessToType inputAccess) printer buildsome Implicit depReason parents inputPath

      let hintReason = "Hint from " <> (BS8.pack . show . targetOutputs) target
      hintedSlaves <- concat <$> mapM (makeSlaves printer buildsome Explicit hintReason parents) (targetAllInputs target)

      let allSlaves = speculativeSlaves ++ hintedSlaves
      unless (null allSlaves) $ waitForSlaves printer parCell buildsome allSlaves

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: Printer -> ParCell -> Buildsome -> Target -> Reason -> Parents -> IO Bool
findApplyExecutionLog printer parCell buildsome target reason parents = do
  mExecutionLog <- readIRef $ Db.executionLog target $ bsDb buildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just executionLog -> do
      res <- tryApplyExecutionLog printer parCell buildsome target reason parents executionLog
      case res of
        Left (str, filePath) -> do
          Printer.putStrLn printer $ concat
            ["Execution log of ", show (targetOutputs target), " did not match because ", str, ": ", show filePath, " changed"]
          return False
        Right () -> return True

showParents :: Parents -> ByteString
showParents = BS8.concat . map showParent
  where
    showParent (targetRep, reason) = BS8.concat ["\n-> ", BS8.pack (show targetRep), " (", reason, ")"]

data TargetDependencyLoop = TargetDependencyLoop Parents deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop parents) = BS8.unpack $ "Target loop: " <> showParents parents

data PanicError = PanicError String deriving (Show, Typeable)
instance E.Exception PanicError

panic :: String -> IO a
panic msg = do
  IO.hPutStrLn IO.stderr msg
  E.throwIO $ PanicError msg

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: Printer -> Buildsome -> Reason -> Parents -> (TargetRep, Target) -> IO Slave
getSlaveForTarget parentPrinter buildsome reason parents (targetRep, target)
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
        , mkSlave newSlaveMVar $ \printer getParId -> do
            parCell <- newIORef =<< getParId
            let release = PoolAlloc.release pool =<< readIORef parCell
            (`finally` release) $ restoreMask $ do
              success <- findApplyExecutionLog printer parCell buildsome target reason parents
              unless success $ buildTarget printer parCell buildsome target reason parents
        )
    where
      pool = bsParallelismPool buildsome
      annotate = annotateException $ "build failure of " ++ show (targetOutputs target) ++ ":\n"
      newParents = (targetRep, reason) : parents
      panicHandler e@E.SomeException {} = panic $ "FAILED during making of slave: " ++ show e
      mkSlave mvar action = do
        (printerId, execution) <-
          E.handle panicHandler $ do
            getParId <- PoolAlloc.startAlloc pool
            printerId <- nextPrinterId buildsome
            printer <- Printer.newFrom parentPrinter printerId
            execution <- async $ annotate $ action printer getParId
            return (printerId, execution)
        let slave = Slave execution printerId (targetOutputs target)
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

buildTarget :: Printer -> ParCell -> Buildsome -> Target -> Reason -> Parents -> IO ()
buildTarget printer parCell buildsome target reason parents =
  targetPrintWrap printer target "BUILDING" reason $ do
    -- TODO: Register each created subdirectory as an output?
    mapM_ (createDirectories . takeDirectory) $ targetOutputs target

    registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
    mapM_ (removeOldOutput printer buildsome registeredOutputs) $ targetOutputs target

    slaves <-
      mkSlavesForPaths printer buildsome Explicit
      ("Hint from " <> (BS8.pack . show . targetOutputs) target) parents
      (targetAllInputs target)
    unless (null slaves) $ waitForSlaves printer parCell buildsome slaves
    inputsRef <- newIORef M.empty
    outputsRef <- newIORef S.empty

    (selfTime, stdOutputs) <-
      Printer.printWrap printer ("runCmd" ++ show (targetOutputs target))
      (runCmd printer parCell buildsome target parents inputsRef outputsRef)
      `finally` do
        outputs <- readIORef outputsRef
        registerOutputs buildsome $ S.intersection outputs $ S.fromList $ targetOutputs target

    outputs <- readIORef outputsRef
    verifyTargetOutputs printer buildsome outputs target

    inputs <- readIORef inputsRef
    saveExecutionLog buildsome target inputs outputs stdOutputs selfTime

    Printer.putStrLn printer $ "Build (now) took " ++ show selfTime ++ " seconds"

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

data CommandFailed = CommandFailed ByteString ExitCode deriving (Typeable)
instance E.Exception CommandFailed
instance Show CommandFailed where
  show (CommandFailed cmd exitCode)
    | '\n' `BS8.elem` cmd = "\"\"\"" <> BS8.unpack cmd <> "\"\"\"" <> suffix
    | otherwise = show cmd <> suffix
    where
      suffix = " failed: " <> show exitCode

shellCmdVerify :: Target -> [String] -> Process.Env -> IO StdOutputs
shellCmdVerify target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand (BS8.unpack cmd)) inheritEnvs newEnvs
  let stdouts = StdOutputs stdout stderr
  printStdouts (show (targetOutputs target)) stdouts
  case exitCode of
    ExitFailure {} -> E.throwIO $ CommandFailed cmd exitCode
    _ -> return stdouts
  where
    cmd = targetCmds target

runCmd ::
  Printer -> ParCell -> Buildsome -> Target -> Parents ->
  -- TODO: Clean this arg list up
  IORef (Map FilePath (AccessType, Reason, Maybe Posix.FileStatus)) ->
  IORef (Set FilePath) -> IO (DiffTime, StdOutputs)
runCmd printer parCell buildsome target parents inputsRef outputsRef = do
  rootPath <- canonicalizePath $ bsRootPath buildsome
  pauseTime <- newIORef 0
  let
    addPauseTime delta = atomicModifyIORef'_ pauseTime (+delta)
    measurePauseTime act = do
      (time, res) <- timeIt act
      addPauseTime time
      return res
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
        slaves <- makeSlavesForAccessType accessType printer buildsome Implicit actDesc parents path
        -- Temporarily paused, so we can temporarily release parallelism
        -- semaphore
        unless (null slaves) $
          Printer.bsPrintWrap printer
          (BS8.concat ["PAUSED: ", (BS8.pack . show . targetOutputs) target, " ", actDesc]) $
          measurePauseTime $ waitForSlaves printer parCell buildsome slaves
    handleOutput _actDesc path
      | outputIgnored path = return ()
      | otherwise = atomicModifyIORef'_ outputsRef $ S.insert path
  (time, stdOutputs) <-
    FSHook.runCommand (bsFsHook buildsome) rootPath
    (timeIt . shellCmdVerify target ["HOME", "PATH"])
    (BS8.pack (show (targetOutputs target)))
    Handlers {..}
  subtractedTime <- (time-) <$> readIORef pauseTime
  return (realToFrac subtractedTime, stdOutputs)

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

standardMakeFilename :: FilePath
standardMakeFilename = "Makefile"

data MakefileScanFailed = MakefileScanFailed deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show MakefileScanFailed = "ERROR: Cannot find a file named " ++ show standardMakeFilename ++ " in this directory or any of its parents"

findMakefile :: IO FilePath
findMakefile = do
  cwd <- Posix.getWorkingDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate takeDirectory cwd
    candidates = map (</> standardMakeFilename) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runEitherT $ mapM_ check candidates
  case res of
    Left found -> makeRelativeToCurrentDirectory found
    Right () -> E.throwIO MakefileScanFailed
  where
    check path = do
      exists <- liftIO $ Posix.fileExist path
      when exists $ left path

data SpecifiedInexistentMakefilePath = SpecifiedInexistentMakefilePath FilePath deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath path) =
    concat ["Specified makefile path: ", show path, " does not exist"]
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: FilePath -> IO FilePath
specifiedMakefile path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath path
    Just stat
      | Posix.isDirectory stat -> return $ path </> "Makefile"
      | otherwise -> return path

data Requested = RequestedClean | RequestedTargets [FilePath] Reason

data BadCommandLine = BadCommandLine String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine msg) = "Invalid command line options: " ++ msg

getRequestedTargets :: [ByteString] -> IO Requested
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
    origCwd <- Posix.getWorkingDirectory
    unless (BS8.null cwd) $ Posix.changeWorkingDirectory cwd
    origMakefile <- Makefile.parse file
    makefile <- Makefile.onMakefilePaths canonicalizePathAsRelative origMakefile
    printer <- Printer.new 0
    Db.with (buildDbFilename file) $ \db ->
      withBuildsome file fsHook db makefile opt $
        \buildsome -> do
        deleteRemovedOutputs buildsome
        let
          inOrigCwd =
            case optMakefilePath opt of
            -- If we found the makefile by scanning upwards, prepend
            -- original cwd to avoid losing it:
            Nothing -> mapM (canonicalizePathAsRelative . (origCwd </>))
            -- Otherwise: there's no useful original cwd:
            Just _ -> return

        requested <- getRequestedTargets $ optRequestedTargets opt
        case requested of
          RequestedTargets requestedTargets reason -> do
            requestedTargetPaths <- inOrigCwd requestedTargets
            Printer.putStrLn printer $ "Building: " ++ intercalate ", " (map show requestedTargetPaths)
            (buildTime, ()) <- timeIt $ want printer buildsome reason requestedTargetPaths
            Printer.putStrLn printer $ "Build Successful: " ++ show buildTime ++ " seconds total."
          RequestedClean -> do
            outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
            leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
            Clean.Result _totalSize totalSpace count <-
              mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
            writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
            writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
            Printer.putStrLn printer $ concat
              [ "Clean Successful: Cleaned "
              , show count, " files freeing an estimated "
              , showBytes (fromIntegral totalSpace)
              ]
