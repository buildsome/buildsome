{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Binary (Binary, get, put)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, partition, nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Filesystem.Path.CurrentOS (encodeString)
import GHC.Generics (Generic)
import Lib.AnnotatedException (annotateException)
import Lib.Async (wrapAsync)
import Lib.Binary (runGet, runPut)
import Lib.ByteString (unprefixed)
import Lib.Directory (getMFileStatus, fileExists, removeFileAllowNotExists)
import Lib.FileDesc (FileDesc, fileDescOfMStat, getFileDesc, FileModeDesc, fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath ((</>), removeRedundantParents)
import Lib.IORef (atomicModifyIORef_, atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), TargetType(..), Target, Pattern)
import Lib.Process (shellCmdVerify)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..))
import System.Argv0 (getArgv0)
import System.FilePath (takeDirectory, (<.>))
import System.Posix.Files (FileStatus)
import System.Posix.Process (getProcessID)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Database.Sophia as Sophia
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.Makefile as Makefile
import qualified Lib.Process as Process
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Directory as Dir

newtype TargetRep = TargetRep FilePath -- We use the minimum output path as the target key/representative
  deriving (Eq, Ord, Show)
computeTargetRep :: Target -> TargetRep
computeTargetRep = TargetRep . minimum . targetOutputs

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]
type Reason = String
type CmdId = ByteString

data Slave = Slave
  { _slaveTarget :: Target
  , slaveExecution :: Async ()
  }

data ExecutingCommand = ExecutingCommand
  { ecCmd :: String
  , ecThreadId :: ThreadId
  , ecTarget :: Target
  , ecParents :: Parents
  , -- For each input file, record modification time before input is
    -- used, to compare it after cmd is done
    ecInputs :: IORef (Map FilePath (AccessType, Maybe FileStatus))
  , ecOutputs :: IORef (Set FilePath)
  , ecActiveConnections :: IORef [MVar ()]
  }

data DirectoryBuildMap = DirectoryBuildMap
  { dbmTargets :: [(TargetRep, Target)]
  , dbmPatterns :: [Pattern]
  }
instance Monoid DirectoryBuildMap where
  mempty = DirectoryBuildMap mempty mempty
  mappend (DirectoryBuildMap x0 x1) (DirectoryBuildMap y0 y1) =
    DirectoryBuildMap (mappend x0 y0) (mappend x1 y1)

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath (TargetRep, Target) -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath DirectoryBuildMap
  }

data Buildsome = Buildsome
  { bsRunningCmds :: IORef (Map CmdId ExecutingCommand)
  , bsSlaveByRepPath :: IORef (Map TargetRep (MVar Slave))
  , bsAddress :: FilePath -- unix socket server
  , bsLdPreloadPath :: FilePath
  , bsDeleteUnspecifiedOutput :: DeleteUnspecifiedOutputs
  , bsBuildMaps :: BuildMaps
  , bsCurJobId :: IORef Int
  , bsRestrictedParallelism :: MSem Int
  , bsDb :: Sophia.Db
  , bsMakefile :: Makefile
  }

slaveWait :: Slave -> IO ()
slaveWait = wait . slaveExecution

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

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

serve :: Buildsome -> Socket -> IO ()
serve buildsome conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> fail $ "Bad connection started with: " ++ show helloLine
    Just pidCmdId -> do
      runningCmds <- readIORef (bsRunningCmds buildsome)
      case M.lookup cmdId runningCmds of
        Nothing -> do
          let cmdIds = M.keys runningCmds
          fail $ "Bad slave id: " ++ show cmdId ++ " mismatches all: " ++ show cmdIds
        Just ec -> handleCmdConnection buildsome conn ec
      where
        [_pidStr, _tidStr, cmdId] = BS.split ':' pidCmdId

maxMsgSize :: Int
maxMsgSize = 8192

handleCmdConnection :: Buildsome -> Socket -> ExecutingCommand -> IO ()
handleCmdConnection buildsome conn ec = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connFinishedMVar <- newEmptyMVar
  atomicModifyIORef_ (ecActiveConnections ec) (connFinishedMVar:)
  protect connFinishedMVar $ do
    sendGo conn
    recvLoop_ maxMsgSize
      (handleCmdMsg buildsome conn ec . Protocol.parseMsg) conn
  where
    protect mvar act = act `E.finally` putMVar mvar ()

recordInput :: ExecutingCommand -> AccessType -> FilePath -> IO ()
recordInput ec accessType path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ (ecInputs ec) $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith
    (\_ (oldAccessType, oldMStat) ->
     (higherAccessType accessType oldAccessType, oldMStat)) path (accessType, mstat)

recordOutput :: ExecutingCommand -> FilePath -> IO ()
recordOutput ec path =
  atomicModifyIORef'_ (ecOutputs ec) $ S.insert path

recordedOutputs :: ExecutingCommand -> IO (Set FilePath)
recordedOutputs = readIORef . ecOutputs

data InvalidCmdOperation = InvalidCmdOperation String
  deriving (Show, Typeable)
instance E.Exception InvalidCmdOperation

data AccessType
  = AccessTypeFull -- open, stat, opendir, etc.  Depend on the content, and if directory, on the file listing
  | AccessTypeModeOnly -- access, readlink.  Depend on its existence/permission-modes only. If directory, does not depend on file listing
higherAccessType :: AccessType -> AccessType -> AccessType
higherAccessType AccessTypeModeOnly AccessTypeModeOnly = AccessTypeModeOnly
higherAccessType _ _ = AccessTypeFull

handleCmdMsg ::
  Buildsome -> Socket -> ExecutingCommand -> Protocol.Func -> IO ()
handleCmdMsg buildsome conn ec msg =
  case msg of
    -- outputs
    Protocol.Open path Protocol.OpenWriteMode _ -> reportOutput path
    Protocol.Open path _ (Protocol.Create _) -> reportOutput path
    Protocol.Creat path _ -> reportOutput path
    Protocol.Rename a b -> reportOutput a >> reportOutput b
    Protocol.Unlink path -> reportOutput path
    Protocol.Truncate path _ -> reportOutput path
    Protocol.Chmod path _ -> reportOutput path
    Protocol.Chown path _ _ -> reportOutput path
    Protocol.MkNod path _ _ -> reportOutput path -- TODO: Special mkNod handling?
    Protocol.MkDir path _ -> reportOutput path
    Protocol.RmDir path -> reportOutput path

    -- I/O
    Protocol.SymLink target linkPath -> reportOutput linkPath >> reportInput AccessTypeFull target
    Protocol.Link src dest ->
      failCmd $ unwords ["Hard links not supported:", show src, "->", show dest]
      -- TODO: Record the fact it's a link
      --reportOutput dest >> reportInput src

    -- inputs
    Protocol.Open path Protocol.OpenReadMode _creationMode -> reportInput AccessTypeFull path
    Protocol.Access path _mode -> reportInput AccessTypeModeOnly path
    Protocol.Stat path -> reportInput AccessTypeFull path
    Protocol.LStat path -> reportInput AccessTypeFull path
    Protocol.OpenDir path -> reportInput AccessTypeFull path
    Protocol.ReadLink path -> reportInput AccessTypeModeOnly path
  where
    failCmd = E.throwTo (ecThreadId ec) . InvalidCmdOperation
    reason = Protocol.showFunc msg ++ " done by " ++ show (ecCmd ec)

    wrapAction = (`E.finally` sendGo conn) . forwardExceptions
    reportInput accessType fullPath =
      wrapAction $
      handleInput accessType =<<
      canonicalizePath fullPath
    reportOutput fullPath =
      wrapAction $
      recordOutput ec =<<
      canonicalizePath fullPath

    forwardExceptions =
      flip E.catch $ \e@E.SomeException {} -> E.throwTo (ecThreadId ec) e
    handleInput accessType path
      | inputIgnored path = sendGo conn
        -- There's no problem for a target to read its own outputs
        -- freely:
      | otherwise = do
        actualOutputs <- recordedOutputs ec
        if path `S.member` actualOutputs
          then sendGo conn
          else do
            slaves <- makeSlavesForAccessType accessType buildsome Implicit reason (ecParents ec) path
            -- Temporarily paused, so we can temporarily release parallelism
            -- semaphore
            unless (null slaves) $ withReleasedParallelism buildsome $
              traverse_ slaveWait slaves
            unless (isLegalOutput (ecTarget ec) path) $
              recordInput ec accessType path

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = Dir.makeRelativeToCurrentDirectory . removeRedundantParents

inputIgnored :: FilePath -> Bool
inputIgnored path = "/dev" `isPrefixOf` path

pairWithTargetRep :: Target -> (TargetRep, Target)
pairWithTargetRep target = (computeTargetRep target, target)

toBuildMaps :: Makefile -> BuildMaps
toBuildMaps makefile = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, target)
      | target <- makefileTargets makefile
      , outputPath <- targetOutputs target
      ]
    childrenMap =
      M.fromListWith mappend $

      [ (takeDirectory outputPath, mempty { dbmTargets = [pairWithTargetRep target] })
      | (outputPath, target) <- outputs
      ] ++

      [ (outPatDir, mempty { dbmPatterns = [targetPattern] })
      | targetPattern <- makefilePatterns makefile
      , outPatDir <- nub (map Makefile.filePatternDirectory (targetOutputs targetPattern))
      ]

    buildMap =
      M.fromListWithKey (\path -> error $ "Overlapping output paths for: " ++ show path)
      [ (outputPath, pairWithTargetRep target)
      | (outputPath, target) <- outputs ]

withBuildsome ::
  Sophia.Db -> Int -> Makefile -> DeleteUnspecifiedOutputs ->
  FilePath -> (Buildsome -> IO a) -> IO a
withBuildsome db parallelism makefile deleteUnspecifiedOutput ldPreloadPath body = do
  runningCmds <- newIORef M.empty
  slaveMapByRepPath <- newIORef M.empty
  bsPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show bsPid
  curJobId <- newIORef 0
  semaphore <- MSem.new parallelism
  let buildsome =
        Buildsome
        { bsRunningCmds = runningCmds
        , bsSlaveByRepPath = slaveMapByRepPath
        , bsAddress = serverFilename
        , bsLdPreloadPath = ldPreloadPath
        , bsBuildMaps = toBuildMaps makefile
        , bsCurJobId = curJobId
        , bsDeleteUnspecifiedOutput = deleteUnspecifiedOutput
        , bsRestrictedParallelism = semaphore
        , bsDb = db
        , bsMakefile = makefile
        }

  withUnixSeqPacketListener serverFilename $ \listener ->
    AsyncContext.new $ \ctx -> do
      _ <- AsyncContext.spawn ctx $ forever $ do
        (conn, _srcAddr) <- Sock.accept listener
        AsyncContext.spawn ctx $ serve buildsome conn
      body buildsome

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  argv0 <- encodeString <$> getArgv0
  Dir.canonicalizePath (takeDirectory argv0 </> "fs_override.so")

nextJobId :: Buildsome -> IO Int
nextJobId buildsome =
  atomicModifyIORef (bsCurJobId buildsome) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: Buildsome -> Explicitness -> Reason -> Parents -> [FilePath] -> IO ()
need buildsome explicitness reason parents paths = do
  slaves <- concat <$> mapM (makeSlaves buildsome explicitness reason parents) paths
  traverse_ slaveWait slaves

assertExists :: Buildsome -> FilePath -> String -> IO ()
assertExists buildsome path msg
  | path `elem` makefilePhonies (bsMakefile buildsome) = return ()
  | otherwise = do
    doesExist <- fileExists path
    unless doesExist $ fail msg

makeDirectSlave :: Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO (Maybe Slave)
makeDirectSlave buildsome explicitness reason parents path =
  case buildMapFind (bsBuildMaps buildsome) path of
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
    -- TODO: Abstract this with buildMapsFindChildren, extract BuildMaps to its own module?
    BuildMaps _ childrenMap = bsBuildMaps buildsome
    DirectoryBuildMap childTargets childPatterns = M.findWithDefault mempty path childrenMap

makeSlavesForAccessType :: AccessType -> Buildsome -> Explicitness -> Reason -> Parents -> FilePath -> IO [Slave]
makeSlavesForAccessType accessType buildsome explicitness reason parents path =
  case accessType of
  AccessTypeFull -> makeSlaves buildsome explicitness reason parents path
  AccessTypeModeOnly -> maybeToList <$> makeDirectSlave buildsome explicitness reason parents path

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

targetKey :: Target -> ByteString
targetKey target =
  MD5.hash $ BS.pack (unlines (targetCmds target)) -- TODO: Canonicalize commands (whitespace/etc)

newtype FileMode = FileMode Int
  deriving (Generic, Show)
instance Binary FileMode

data InputAccess = InputAccessModeOnly FileModeDesc | InputAccessFull FileDesc
  deriving (Generic, Show)
instance Binary InputAccess

inputAccessToType :: InputAccess -> AccessType
inputAccessToType InputAccessModeOnly {} = AccessTypeModeOnly
inputAccessToType InputAccessFull {} = AccessTypeFull

data ExecutionLog = ExecutionLog
  { _elInputsDescs :: Map FilePath InputAccess
  , _elOutputsDescs :: Map FilePath FileDesc
  } deriving (Generic, Show)
instance Binary ExecutionLog

setKey :: Binary a => Buildsome -> ByteString -> a -> IO ()
setKey buildsome key val = Sophia.setValue (bsDb buildsome) key $ runPut $ put val

getKey :: Binary a => Buildsome -> ByteString -> IO (Maybe a)
getKey buildsome key = fmap (runGet get) <$> Sophia.getValue (bsDb buildsome) key

saveExecutionLog :: Buildsome -> Target -> Map FilePath (AccessType, Maybe FileStatus) -> Set FilePath -> IO ()
saveExecutionLog buildsome target inputs outputs = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM (S.toList outputs) $ \outPath -> do
      fileDesc <- getFileDesc outPath
      return (outPath, fileDesc)
  let execLog = ExecutionLog inputsDescs (M.fromList outputDescPairs)
  setKey buildsome (targetKey target) execLog
  where
    inputAccess path (AccessTypeFull, mStat) = InputAccessFull <$> fileDescOfMStat path mStat
    inputAccess path (AccessTypeModeOnly, mStat) = InputAccessModeOnly <$> fileModeDescOfMStat path mStat

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

applyExecutionLog ::
  Buildsome -> Target -> Parents -> ExecutionLog ->
  IO (Either (String, FilePath) ())
applyExecutionLog buildsome target parents (ExecutionLog inputsDescs outputsDescs) =
  runEitherT $ do
    liftIO waitForInputs

    forM_ (M.toList inputsDescs) $ \(filePath, oldInputAccess) ->
      case oldInputAccess of
        InputAccessFull oldDesc ->         compareToNewDesc "input"       getFileDesc     filePath oldDesc
        InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc filePath oldModeDesc
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    forM_ (M.toList outputsDescs) $ \(filePath, oldDesc) -> do
      compareToNewDesc "output" getFileDesc filePath oldDesc

    liftIO $ verifyTargetOutputs buildsome (M.keysSet outputsDescs) target
    where
      compareToNewDesc str getNewDesc filePath oldDesc = do
        newDesc <- liftIO $ getNewDesc filePath
        when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation
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

        traverse_ slaveWait (speculativeSlaves ++ hintedSlaves)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: Buildsome -> Target -> Parents -> IO Bool
findApplyExecutionLog buildsome target parents = do
  mExecutionLog <- getKey buildsome (targetKey target)
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just executionLog -> do
      res <- applyExecutionLog buildsome target parents executionLog
      case res of
        Left (str, filePath) -> do
          putStrLn $ concat
            ["Execution log of ", show (targetOutputs target), " did not match because ", str, ": ", show filePath, " changed"]
          return False
        Right () -> do
          putStrLn $ "Execution log match for: " ++ show (targetOutputs target)
          return True

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
    then Slave target <$> async (return ())
    else do
      execution <- async . annotate . restoreMask $ do
        mapM_ removeFileAllowNotExists $ targetOutputs target
        need buildsome Explicit
          ("Hint from " ++ show (take 1 (targetOutputs target))) parents
          (targetAllInputs target)
        inputsRef <- newIORef M.empty
        outputsRef <- newIORef S.empty
        withAllocatedParallelism buildsome $
          mapM_ (runCmd buildsome target reason parents inputsRef outputsRef)
            (targetCmds target)
        inputs <- readIORef inputsRef
        outputs <- readIORef outputsRef
        verifyTargetOutputs buildsome outputs target
        saveExecutionLog buildsome target inputs outputs
      return $ Slave target execution
  where
    annotate = annotateException ("build failure of " ++ show (targetOutputs target))

registeredOutputsKey :: ByteString
registeredOutputsKey = BS.pack "outputs"

getRegisteredOutputs :: Buildsome -> IO [FilePath]
getRegisteredOutputs buildsome =
  fromMaybe [] <$> getKey buildsome registeredOutputsKey

setRegisteredOutputs :: Buildsome -> [FilePath] -> IO ()
setRegisteredOutputs buildsome outputs =
  setKey buildsome registeredOutputsKey outputs

registerOutputs :: Buildsome -> [FilePath] -> IO ()
registerOutputs buildsome outputPaths = do
  outputs <- getRegisteredOutputs buildsome
  setRegisteredOutputs buildsome $ outputPaths ++ outputs

buildMapFind :: BuildMaps -> FilePath -> Maybe (TargetRep, Target)
buildMapFind (BuildMaps buildMap childrenMap) outputPath =
  -- Allow specific/direct matches to override pattern matches
  directMatch `mplus` patternMatch
  where
    directMatch = outputPath `M.lookup` buildMap
    patterns = dbmPatterns $ M.findWithDefault mempty (takeDirectory outputPath) childrenMap
    patternMatch =
      case mapMaybe (Makefile.instantiatePatternByOutput outputPath) patterns of
      [] -> Nothing
      [target] -> Just (computeTargetRep target, target)
      targets ->
        error $ concat
        [ "Multiple matching patterns: ", show outputPath
        , " (", show (map targetOutputs targets), ")"
        ]

deleteRemovedOutputs :: Buildsome -> IO ()
deleteRemovedOutputs buildsome = do
  outputs <- getRegisteredOutputs buildsome
  liveOutputs <-
    fmap concat .
    forM outputs $ \output ->
      case buildMapFind (bsBuildMaps buildsome) output of
      Just _ -> return [output]
      Nothing -> do
        putStrLn $ "Removing old output: " ++ show output
        removeFileAllowNotExists output
        return []
  setRegisteredOutputs buildsome liveOutputs

runCmd ::
  Buildsome -> Target -> Reason -> Parents ->
  -- TODO: Clean this arg list up
  IORef (Map FilePath (AccessType, Maybe FileStatus)) ->
  IORef (Set FilePath) ->
  String -> IO ()
runCmd buildsome target reason parents inputsRef outputsRef cmd = do
  registerOutputs buildsome (targetOutputs target)

  activeConnections <- newIORef []

  cmdIdNum <- nextJobId buildsome
  let cmdId = BS.pack ("cmd" ++ show cmdIdNum)
  tid <- myThreadId
  let ec = ExecutingCommand cmd tid target parents inputsRef outputsRef activeConnections
  putStrLn $ concat ["{ ", show cmd, ": ", reason]
  atomicModifyIORef_ (bsRunningCmds buildsome) $ M.insert cmdId ec
  shellCmdVerify ["HOME", "PATH"] (mkEnvVars buildsome cmdId) cmd
  putStrLn $ concat ["} ", show cmd]

  -- Give all connections a chance to complete and perhaps fail
  -- this execution:
  mapM_ readMVar =<< readIORef activeConnections

mkEnvVars :: Buildsome -> ByteString -> Process.Env
mkEnvVars buildsome cmdId =
    [ ("LD_PRELOAD", bsLdPreloadPath buildsome)
    , ("EFBUILD_MASTER_UNIX_SOCKADDR", bsAddress buildsome)
    , ("EFBUILD_CMD_ID", BS.unpack cmdId)
    ]

withDb :: FilePath -> (Sophia.Db -> IO a) -> IO a
withDb dbFileName body = do
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation dbFileName
    Sophia.withDb env $ \db ->
      body db

main :: IO ()
main = do
  Opt makefileName mparallelism deleteUnspecifiedOutput <- getOpt
  let buildDbFilename = makefileName <.> "db"
      parallelism = fromMaybe 1 mparallelism
  makefile <- Makefile.parse makefileName
  withDb buildDbFilename $ \db -> do
    ldPreloadPath <- getLdPreloadPath
    withBuildsome db parallelism makefile deleteUnspecifiedOutput ldPreloadPath $
      \buildsome -> do
      deleteRemovedOutputs buildsome
      case makefileTargets makefile of
        [] -> putStrLn "Empty makefile, done nothing..."
        (target:_) ->
          need buildsome Explicit "First target in Makefile" [] $
          take 1 (targetOutputs target)
