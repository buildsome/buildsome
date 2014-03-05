module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, partition)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid
import Data.Set (Set)
import Data.Traversable (traverse)
import Db (Db)
import Lib.AccessType (AccessType(..))
import Lib.AnnotatedException (annotateException)
import Lib.Async (wrapAsync)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.Directory (getMFileStatus, fileExists, removeFileAllowNotExists)
import Lib.FSHook (FSHook)
import Lib.FileDesc (fileDescOfMStat, getFileDesc, fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath ((</>), canonicalizePath)
import Lib.IORef (atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), TargetType(..), Target)
import Lib.StdOutputs (StdOutputs, printStdouts)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..))
import System.FilePath (takeDirectory, (<.>))
import System.Posix.Files (FileStatus)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Db
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.FSHook as FSHook
import qualified Lib.Makefile as Makefile
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

withBuildsome :: Db -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
withBuildsome db makefile opt body = do
  slaveMapByRepPath <- newIORef M.empty
  semaphore <- MSem.new parallelism
  FSHook.with $ \fsHook -> do
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
      `E.finally` maybeUpdateGitIgnore buildsome
  where
    maybeUpdateGitIgnore buildsome
      | writeGitIgnore = updateGitIgnore buildsome makefilePath
      | otherwise = return ()
    parallelism = fromMaybe 1 mParallelism
    Opt makefilePath mParallelism writeGitIgnore deleteUnspecifiedOutput = opt

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- Db.readRegisteredOutputs (bsDb buildsome)
  let gitIgnorePath = takeDirectory makefilePath </> ".gitignore"
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  writeFile gitIgnorePath $ unlines $ extraIgnored ++ S.toList outputs

need :: Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO ()
need buildsome explicitness reason parents paths = do
  slaves <- concat <$> mapM (makeSlaves buildsome explicitness reason parents) paths
  mapM_ slaveWait slaves

assertExists :: Buildsome -> FilePath -> String -> IO ()
assertExists buildsome path msg
  | path `elem` makefilePhonies (bsMakefile buildsome) = return ()
  | otherwise = do
    doesExist <- fileExists path
    unless doesExist $ fail msg

makeDirectSlave :: Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO (Maybe Slave)
makeDirectSlave buildsome explicitness reason parents path =
  case BuildMaps.find (bsBuildMaps buildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $
      assertExists buildsome path $
      concat ["No rule to build ", show path, " (", reason, ")"]
    return Nothing
  Just tgt -> do
    slave <- getSlaveForTarget buildsome reason parents tgt
    Just <$> case explicitness of
      Implicit -> return slave
      Explicit -> verifyFileGetsCreated slave
  where
    verifyFileGetsCreated slave = do
      wrappedExecution <-
        wrapAsync (slaveExecution slave) $ \() ->
        assertExists buildsome path $ concat
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

handleLegalUnspecifiedOutputs :: DeleteUnspecifiedOutputs -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs policy target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ putStrLn $ concat
    [ "WARNING: Removing leaked unspecified outputs: "
    , show paths, " from target for: ", show (targetOutputs target) ]
  case policy of
    DeleteUnspecifiedOutputs -> mapM_ Dir.removeFile paths
    DontDeleteUnspecifiedOutputs -> return ()

-- Verify output of whole of slave/execution log
verifyTargetOutputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs buildsome outputs target = do

  let (unspecifiedOutputs, illegalOutputs) = partition (isLegalOutput target) allUnspecified

  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs
    (bsDeleteUnspecifiedOutput buildsome) target =<<
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
  [StdOutputs] -> IO ()
saveExecutionLog buildsome target inputs outputs stdOutputs = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM (S.toList outputs) $ \outPath -> do
      fileDesc <- getFileDesc outPath
      return (outPath, fileDesc)
  Db.writeExecutionLog (bsDb buildsome) target $
    Db.ExecutionLog inputsDescs (M.fromList outputDescPairs) stdOutputs
  where
    inputAccess path (AccessTypeFull, mStat) = Db.InputAccessFull <$> fileDescOfMStat path mStat
    inputAccess path (AccessTypeModeOnly, mStat) = Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

-- Already verified that the execution log is a match
applyExecutionLog ::
  Buildsome -> TargetType FilePath FilePath ->
  Set FilePath -> [StdOutputs] -> IO ()
applyExecutionLog buildsome target outputs stdOutputs
  | length (targetCmds target) /= length stdOutputs =
    fail $ unwords
    ["Invalid recorded standard outputs:", show target, show stdOutputs]

  | otherwise = do
    forM_ (zip (targetCmds target) stdOutputs) $ \(cmd, outs) -> do
      putStrLn $ "{ REPLAY of " ++ show cmd
      printStdouts outs

    verifyTargetOutputs buildsome outputs target

tryApplyExecutionLog ::
  Buildsome -> Target -> Parents -> Db.ExecutionLog ->
  IO (Either (String, FilePath) ())
tryApplyExecutionLog buildsome target parents (Db.ExecutionLog inputsDescs outputsDescs stdOutputs) = do
  waitForInputs
  runEitherT $ do
    forM_ (M.toList inputsDescs) $ \(filePath, oldInputAccess) ->
      case oldInputAccess of
        Db.InputAccessFull oldDesc ->         compareToNewDesc "input"       getFileDesc     filePath oldDesc
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc filePath oldModeDesc
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    forM_ (M.toList outputsDescs) $ \(filePath, oldDesc) -> do
      compareToNewDesc "output" getFileDesc filePath oldDesc

    liftIO $
      applyExecutionLog buildsome target
      (M.keysSet outputsDescs) stdOutputs
  where
    compareToNewDesc str getNewDesc filePath oldDesc = do
      newDesc <- liftIO $ getNewDesc filePath
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation
    inputAccessToType Db.InputAccessModeOnly {} = AccessTypeModeOnly
    inputAccessToType Db.InputAccessFull {} = AccessTypeFull
    waitForInputs = do
      -- TODO: This is good for parallelism, but bad if the set of
      -- inputs changed, as it may build stuff that's no longer
      -- required:

      let reason = "Recorded dependency of " ++ show (targetOutputs target)
      speculativeSlaves <-
        fmap concat $ forM (M.toList inputsDescs) $ \(inputPath, inputAccess) ->
        makeSlavesForAccessType (inputAccessToType inputAccess) buildsome Implicit reason parents inputPath

      let hintReason = "Hint from " ++ show (take 1 (targetOutputs target))
      hintedSlaves <- concat <$> mapM (makeSlaves buildsome Explicit hintReason parents) (targetAllInputs target)

      mapM_ slaveWait (speculativeSlaves ++ hintedSlaves)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: Buildsome -> Target -> Parents -> IO Bool
findApplyExecutionLog buildsome target parents = do
  mExecutionLog <- Db.readExecutionLog (bsDb buildsome) target
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just executionLog -> do
      res <- tryApplyExecutionLog buildsome target parents executionLog
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
    E.mask $ \restoreMask -> do
      getSlave <-
        atomicModifyIORef (bsSlaveByRepPath buildsome) $
        \oldSlaveMap ->
        case M.lookup targetRep oldSlaveMap of
        Nothing ->
          ( M.insert targetRep newSlaveMVar oldSlaveMap
          , resultIntoMVar newSlaveMVar =<<
            spawnSlave buildsome target reason newParents restoreMask
          )
        Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
      getSlave
    where
      newParents = (targetRep, reason) : parents
      resultIntoMVar mvar x = putMVar mvar x >> return x

-- Spawn a new slave for a target
spawnSlave :: Buildsome -> Target -> Reason -> Parents -> (IO () -> IO ()) -> IO Slave
spawnSlave buildsome target reason parents restoreMask = do
  success <- findApplyExecutionLog buildsome target parents
  if success
    then Slave <$> async (return ())
    else do
      execution <- async . annotate . restoreMask $ do
        putStrLn $ concat ["{ ", show (targetOutputs target), " (", reason, ")"]
        mapM_ removeFileAllowNotExists $ targetOutputs target
        need buildsome Explicit
          ("Hint from " ++ show (take 1 (targetOutputs target))) parents
          (targetAllInputs target)
        inputsRef <- newIORef M.empty
        outputsRef <- newIORef S.empty
        stdOutputs <-
          withAllocatedParallelism buildsome $
          mapM (runCmd buildsome target parents inputsRef outputsRef)
          (targetCmds target)
        inputs <- readIORef inputsRef
        outputs <- readIORef outputsRef
        registerOutputs buildsome $ S.intersection outputs $ S.fromList $ targetOutputs target
        verifyTargetOutputs buildsome outputs target
        saveExecutionLog buildsome target inputs outputs stdOutputs
        putStrLn $ concat ["} ", show (targetOutputs target)]
      return $ Slave execution
  where
    annotate = annotateException ("build failure of " ++ show (targetOutputs target))

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs buildsome outputPaths = do
  outputs <- Db.readRegisteredOutputs (bsDb buildsome)
  Db.writeRegisteredOutputs (bsDb buildsome) $ outputPaths <> outputs

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
  Db.writeRegisteredOutputs (bsDb buildsome) liveOutputs

runCmd ::
  Buildsome -> Target -> Parents ->
  -- TODO: Clean this arg list up
  IORef (Map FilePath (AccessType, Maybe FileStatus)) ->
  IORef (Set FilePath) ->
  String -> IO StdOutputs
runCmd buildsome target parents inputsRef outputsRef cmd = do
  putStrLn $ concat ["  { ", show cmd, ": "]
  let
    handleInputRaw accessType actDesc rawPath =
      handleInput accessType actDesc =<< canonicalizePath rawPath
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
    handleOutputRaw actDesc rawPath =
      handleOutput actDesc =<< canonicalizePath rawPath
    handleOutput _actDesc path =
      atomicModifyIORef'_ outputsRef $ S.insert path

  stdOutputs <-
    FSHook.runCommand (bsFsHook buildsome) cmd
    handleInputRaw handleOutputRaw
  putStrLn $ concat ["  } ", show cmd]
  return stdOutputs

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

main :: IO ()
main = do
  opt <- getOpt
  makefile <- Makefile.parse (optMakefilePath opt)
  Db.with (buildDbFilename (optMakefilePath opt)) $ \db -> do
    withBuildsome db makefile opt $
      \buildsome -> do
      deleteRemovedOutputs buildsome
      case makefileTargets makefile of
        [] -> putStrLn "Empty makefile, done nothing..."
        (target:_) ->
          need buildsome Explicit "First target in Makefile" [] $
          take 1 (targetOutputs target)
