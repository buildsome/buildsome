module Main (main) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, partition, intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid
import Data.Set (Set)
import Data.Traversable (traverse)
import Db (Db, IRef(..))
import Lib.AccessType (AccessType(..))
import Lib.AnnotatedException (annotateException)
import Lib.Async (wrapAsync)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.Directory (getMFileStatus, fileExists, removeFileAllowNotExists)
import Lib.FSHook (FSHook)
import Lib.FileDesc (fileDescOfMStat, getFileDesc, fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath ((</>), canonicalizePath, splitFileName)
import Lib.IORef (atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), TargetType(..), Target)
import Lib.ShowBytes (showBytes)
import Lib.StdOutputs (StdOutputs(..), printStdouts)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..))
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (<.>), makeRelative)
import System.Posix.Files (FileStatus)
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
import qualified Lib.Process as Process
import qualified System.Directory as Dir

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]
type Reason = String

newtype Slave = Slave { slaveExecution :: Async () }

data Buildsome = Buildsome
  { bsSlaveByRepPath :: IORef (Map TargetRep (MVar Slave))
  , bsDeleteUnspecifiedOutput :: DeleteUnspecifiedOutputs
  , bsBuildMaps :: BuildMaps
  , bsRestrictedParallelism :: MSem Int
  , bsDb :: Db
  , bsMakefile :: Makefile
  , bsFsHook :: FSHook
  }

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

isLegalOutput :: Target -> FilePath -> Bool
isLegalOutput target path =
  path `elem` targetOutputs target ||
  allowedUnspecifiedOutput path

recordInput :: IORef (Map FilePath (AccessType, Maybe FileStatus)) -> AccessType -> FilePath -> IO ()
recordInput inputsRef accessType path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith
    (\_ (oldAccessType, oldMStat) ->
     (max accessType oldAccessType, oldMStat)) path (accessType, mstat)

inputIgnored :: FilePath -> Bool
inputIgnored path = "/dev" `isPrefixOf` path

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
  let
    buildsome =
      Buildsome
      { bsSlaveByRepPath = slaveMapByRepPath
      , bsBuildMaps = BuildMaps.make makefile
      , bsDeleteUnspecifiedOutput = deleteUnspecifiedOutput
      , bsRestrictedParallelism = semaphore
      , bsDb = db
      , bsMakefile = makefile
      , bsFsHook = fsHook
      }
  body buildsome
    `E.finally` do
      maybeUpdateGitIgnore buildsome
      -- We must not leak running slaves as we're not allowed to
      -- access fsHook, db, etc after leaving here:
      cancelAllSlaves buildsome
  where
    maybeUpdateGitIgnore buildsome
      | writeGitIgnore = updateGitIgnore buildsome makefilePath
      | otherwise = return ()
    parallelism = fromMaybe 1 mParallelism
    Opt _targets _mMakefile mParallelism writeGitIgnore deleteUnspecifiedOutput = opt

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- Db.readRegisteredOutputs (bsDb buildsome)
  let dir = takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  writeFile gitIgnorePath $ unlines $
    map (makeRelative dir) $
    extraIgnored ++ S.toList outputs

need :: Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO ()
need buildsome explicitness reason parents paths = do
  slaves <- concat <$> mapM (makeSlaves buildsome explicitness reason parents) paths
  mapM_ slaveWait slaves

want :: Buildsome -> Reason -> [FilePath] -> IO ()
want buildsome reason = need buildsome Explicit reason []

assertExists :: FilePath -> String -> IO ()
assertExists path msg = do
  doesExist <- fileExists path
  unless doesExist $ fail msg

makeDirectSlave :: Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO (Maybe Slave)
makeDirectSlave buildsome explicitness reason parents path =
  case BuildMaps.find (bsBuildMaps buildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $
      assertExists path $
      concat ["No rule to build ", show path, " (", reason, ")"]
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
        assertExists path $ concat
          [ show path
          , " explicitly demanded but was not "
          , "created by its target rule" ]
      return slave { slaveExecution = wrappedExecution }

makeChildSlaves :: Buildsome -> Reason -> Parents -> FilePath -> IO [Slave]
makeChildSlaves buildsome reason parents path
  | not (null childPatterns) =
    fail "Read directory on directory with patterns: Enumeration of pattern outputs not supported yet"
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

handleLegalUnspecifiedOutputs :: Buildsome -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs buildsome target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ putStrLn $ concat
    [ "WARNING: Leaked unspecified outputs: "
    , show paths, " from target for: ", show (targetOutputs target)
    , ", ", actionDesc
    ]
  action
  where
    (actionDesc, action) =
      case bsDeleteUnspecifiedOutput buildsome of
      DeleteUnspecifiedOutputs -> ("deleting", mapM_ Dir.removeFile paths)
      DontDeleteUnspecifiedOutputs -> ("keeping", registerOutputs buildsome (S.fromList paths))

-- Verify output of whole of slave/execution log
verifyTargetOutputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs buildsome outputs target = do

  let (unspecifiedOutputs, illegalOutputs) = partition (isLegalOutput target) allUnspecified

  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs buildsome target =<<
    filterM fileExists unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM fileExists illegalOutputs
  unless (null existingIllegalOutputs) $ do
    putStrLn $ "Illegal output files created: " ++ show existingIllegalOutputs
    mapM_ removeFileAllowNotExists existingIllegalOutputs
    fail $ concat
      [ "Target for ", show (targetOutputs target)
      , " wrote to unspecified output files: ", show existingIllegalOutputs
      , ", allowed outputs: ", show specified ]
  unless (S.null unusedOutputs) $
    putStrLn $ "WARNING: Over-specified outputs: " ++ show (S.toList unusedOutputs)
  where
    phonies = S.fromList $ makefilePhonies $ bsMakefile buildsome
    unusedOutputs = (specified `S.difference` outputs) `S.difference` phonies
    allUnspecified = S.toList $ outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

saveExecutionLog ::
  Buildsome -> Target -> Map FilePath (AccessType, Maybe FileStatus) -> Set FilePath ->
  StdOutputs -> IO ()
saveExecutionLog buildsome target inputs outputs stdOutputs = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM (S.toList outputs) $ \outPath -> do
      fileDesc <- getFileDesc outPath
      return (outPath, fileDesc)
  writeIRef (Db.executionLog target (bsDb buildsome)) $
    Db.ExecutionLog inputsDescs (M.fromList outputDescPairs) stdOutputs
  where
    inputAccess path (AccessTypeFull, mStat) = Db.InputAccessFull <$> fileDescOfMStat path mStat
    inputAccess path (AccessTypeModeOnly, mStat) = Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

targetPrintWrap :: Target -> String -> Reason -> IO a -> IO a
targetPrintWrap target str reason body =
  putStrLn before *> body <* putStrLn after
  where
    cmd = targetCmds target
    before = concat $
      [ "{ ", show (targetOutputs target), " ", str, " (", reason, ")" ] ++
      [ "\n" ++ indent cmd | not (null cmd) ]
    after  = concat ["} ", show (targetOutputs target)]

-- Already verified that the execution log is a match
applyExecutionLog ::
  Buildsome -> Target -> Reason -> Set FilePath -> StdOutputs -> IO ()
applyExecutionLog buildsome target reason outputs stdOutputs =
  targetPrintWrap target "REPLAY" reason $ do
    printStdouts (show (targetOutputs target)) stdOutputs
    verifyTargetOutputs buildsome outputs target

tryApplyExecutionLog ::
  Buildsome -> Target -> Reason -> Parents -> Db.ExecutionLog ->
  IO (Either (String, FilePath) ())
tryApplyExecutionLog buildsome target reason parents executionLog = do
  waitForInputs
  runEitherT $ do
    forM_ (M.toList inputsDescs) $ \(filePath, oldInputAccess) ->
      case oldInputAccess of
        Db.InputAccessFull oldDesc ->         compareToNewDesc "input"       getFileDesc     (filePath, oldDesc)
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc (filePath, oldModeDesc)
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    mapM_ (compareToNewDesc "output" getFileDesc) $ M.toList outputsDescs

    liftIO $
      applyExecutionLog buildsome target reason
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

      let depReason = "Recorded dependency of " ++ show (targetOutputs target)
      speculativeSlaves <-
        fmap concat $ forM (M.toList inputsDescs) $ \(inputPath, inputAccess) ->
        makeSlavesForAccessType (inputAccessToType inputAccess) buildsome Implicit depReason parents inputPath

      let hintReason = "Hint from " ++ show (take 1 (targetOutputs target))
      hintedSlaves <- concat <$> mapM (makeSlaves buildsome Explicit hintReason parents) (targetAllInputs target)

      mapM_ slaveWait (speculativeSlaves ++ hintedSlaves)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: Buildsome -> Target -> Reason -> Parents -> IO Bool
findApplyExecutionLog buildsome target reason parents = do
  mExecutionLog <- readIRef $ Db.executionLog target $ bsDb buildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just executionLog -> do
      res <- tryApplyExecutionLog buildsome target reason parents executionLog
      case res of
        Left (str, filePath) -> do
          putStrLn $ concat
            ["Execution log of ", show (targetOutputs target), " did not match because ", str, ": ", show filePath, " changed"]
          return False
        Right () -> return True

showParents :: Parents -> String
showParents = concatMap showParent
  where
    showParent (targetRep, reason) = concat ["\n-> ", show targetRep, " (", reason, ")"]

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: Buildsome -> Reason -> Parents -> (TargetRep, Target) -> IO Slave
getSlaveForTarget buildsome reason parents (targetRep, target)
  | any ((== targetRep) . fst) parents = fail $ "Target loop: " ++ showParents newParents
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
            success <- findApplyExecutionLog buildsome target reason parents
            unless success $ buildTarget buildsome target reason parents
        )
    where
      annotate = annotateException $ "build failure of " ++ show (targetOutputs target) ++ ":\n"
      newParents = (targetRep, reason) : parents
      mkSlave mvar action = do
        slave <- fmap Slave . async $ annotate action
        putMVar mvar slave >> return slave

buildTarget :: Buildsome -> Target -> Reason -> Parents -> IO ()
buildTarget buildsome target reason parents =
  targetPrintWrap target "BUILDING" reason $ do
    -- TODO: Register each created subdirectory as an output?
    mapM_ (Dir.createDirectoryIfMissing True . takeDirectory) $ targetOutputs target

    mapM_ removeFileAllowNotExists $ targetOutputs target
    need buildsome Explicit
      ("Hint from " ++ show (take 1 (targetOutputs target))) parents
      (targetAllInputs target)
    inputsRef <- newIORef M.empty
    outputsRef <- newIORef S.empty
    stdOutputs <-
      withAllocatedParallelism buildsome $
      runCmd buildsome target parents inputsRef outputsRef
    inputs <- readIORef inputsRef
    outputs <- readIORef outputsRef
    registerOutputs buildsome $ S.intersection outputs $ S.fromList $ targetOutputs target
    verifyTargetOutputs buildsome outputs target
    saveExecutionLog buildsome target inputs outputs stdOutputs

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs buildsome outputPaths = do
  outputs <- Db.readRegisteredOutputs (bsDb buildsome)
  writeIRef (Db.registeredOutputs (bsDb buildsome)) $
    outputPaths <> outputs

deleteRemovedOutputs :: Buildsome -> IO ()
deleteRemovedOutputs buildsome = do
  outputs <- Db.readRegisteredOutputs (bsDb buildsome)
  liveOutputs <-
    fmap mconcat .
    forM (S.toList outputs) $ \output ->
      case BuildMaps.find (bsBuildMaps buildsome) output of
      Just _ -> return $ S.singleton output
      Nothing -> do
        putStrLn $ "Removing old output: " ++ show output
        removeFileAllowNotExists output
        return S.empty
  writeIRef (Db.registeredOutputs (bsDb buildsome)) liveOutputs

shellCmdVerify :: Target -> [String] -> Process.Env -> IO StdOutputs
shellCmdVerify target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand cmd) inheritEnvs newEnvs
  let stdouts = StdOutputs stdout stderr
  printStdouts (show (targetOutputs target)) stdouts
  case exitCode of
    ExitFailure {} -> fail $ "\"\"\"\n" ++ cmd ++ "\"\"\" failed!"
    _ -> return stdouts
  where
    cmd = targetCmds target

runCmd ::
  Buildsome -> Target -> Parents ->
  -- TODO: Clean this arg list up
  IORef (Map FilePath (AccessType, Maybe FileStatus)) ->
  IORef (Set FilePath) -> IO StdOutputs
runCmd buildsome target parents inputsRef outputsRef =
  FSHook.runCommand (bsFsHook buildsome)
  (shellCmdVerify target ["HOME", "PATH"])
  (show (targetOutputs target))
  handleInputRaw handleOutputRaw
  where
    handleInput accessType actDesc path
      | inputIgnored path = return ()
      | otherwise = do
        actualOutputs <- readIORef outputsRef
        -- There's no problem for a target to read its own outputs freely:
        unless (path `S.member` actualOutputs) $ do
          slaves <- makeSlavesForAccessType accessType buildsome Implicit actDesc parents path
          -- Temporarily paused, so we can temporarily release parallelism
          -- semaphore
          unless (null slaves) $ withReleasedParallelism buildsome $
            mapM_ slaveWait slaves
          unless (isLegalOutput target path) $
            recordInput inputsRef accessType path
    handleOutput _actDesc path =
      atomicModifyIORef'_ outputsRef $ S.insert path

    handleInputRaw accessType actDesc rawCwd rawPath =
      handleInput accessType actDesc =<<
      canonicalizePath (rawCwd </> rawPath)
    handleOutputRaw actDesc rawCwd rawPath =
      handleOutput actDesc =<<
      canonicalizePath (rawCwd </> rawPath)

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

findMakefile :: IO FilePath
findMakefile = do
  cwd <- Dir.getCurrentDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate takeDirectory cwd
    candidates = map (</> filename) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runEitherT $ mapM_ check candidates
  case res of
    Left found -> Dir.makeRelativeToCurrentDirectory found
    Right () -> fail $ "Cannot find a file named " ++ show filename ++ " in this directory or any of its parents"
  where
    filename = "Makefile"
    check path = do
      exists <- liftIO $ Dir.doesFileExist path
      when exists $ left path

specifiedMakefile :: FilePath -> IO FilePath
specifiedMakefile path = do
  f <- Dir.doesFileExist path
  d <- Dir.doesDirectoryExist path
  case (f, d) of
    (True, _) -> return path
    (_, True) -> return $ path </> "Makefile"
    _ -> fail $ "Cannot find specified Makefile: " ++ show path

data Requested = RequestedClean | RequestedTargets [FilePath] Reason

getRequestedTargets :: [String] -> IO Requested
getRequestedTargets ["clean"] = return RequestedClean
getRequestedTargets [] = return $ RequestedTargets ["default"] "implicit 'default' target"
getRequestedTargets ts
  | "clean" `elem` ts = fail "Clean must be requested exclusively"
  | otherwise = return $ RequestedTargets ts "explicit request from cmdline"

main :: IO ()
main = FSHook.with $ \fsHook -> do
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
          outputs <- Db.readRegisteredOutputs (bsDb buildsome)
          Clean.Result _totalSize totalSpace count <- mconcat <$> mapM Clean.output (S.toList outputs)
          writeIRef (Db.registeredOutputs (bsDb buildsome)) S.empty
          putStrLn $ concat
            [ "Clean Successful: Cleaned "
            , show count, " files freeing an estimated "
            , showBytes (fromIntegral totalSpace)
            ]
