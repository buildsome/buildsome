{-# LANGUAGE DeriveGeneric #-}
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary, get, put)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList, isJust)
import Data.Set (Set)
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import Lib.Binary (runGet, runPut)
import Lib.ByteString (unprefixed)
import Lib.Directory (getMFileStatus, fileExists, catchDoesNotExist)
import Lib.IORef (atomicModifyIORef_, atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), Target(..), makefileParser)
import Lib.Process (shellCmdVerify)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import Opts (getOpt, Opt(..), DeleteUnspecifiedOutputs(..))
import System.Environment (getProgName)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (FileStatus, isRegularFile, isDirectory, isSymbolicLink, modificationTime, readSymbolicLink)
import System.Posix.Process (getProcessID)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Database.Sophia as Sophia
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.Process as Process
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Directory as Dir

type ContentHash = ByteString

data FileDesc
  = RegularFile ContentHash
  | Symlink FilePath
  | Directory -- files inside have their own FileDesc's
  | NoFile -- an unlinked/deleted file at a certain path is also a
           -- valid input or output of a build step
  deriving (Generic, Eq, Show)
instance Binary FileDesc

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
  , -- For each input file, record modification time before input is
    -- used, to compare it after cmd is done
    ecInputs :: IORef (Map FilePath (Maybe FileStatus))
  , ecOutputs :: IORef (Set FilePath)
  , ecActiveConnections :: IORef [MVar ()]
  }

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath (FilePath, Target) -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath [(FilePath, Target)] -- parent/dir paths -> all build steps that build directly into it
  }

data Buildsome = Buildsome
  { bsRunningCmds :: IORef (Map CmdId ExecutingCommand)
  , bsSlaveByRepPath :: IORef (Map FilePath (MVar Slave))
  , bsAddress :: FilePath -- unix socket server
  , bsLdPreloadPath :: FilePath
  , bsDeleteUnspecifiedOutput :: DeleteUnspecifiedOutputs
  , bsBuildMaps :: BuildMaps
  , bsCurJobId :: IORef Int
  , bsRestrictedParallelism :: MSem Int
  , bsDb :: Sophia.Db
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
allowedUnspecifiedOutput path = or
  [ "/tmp" `isPrefixOf` path
  , ".pyc" `isSuffixOf` path
  ]

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
        Just executingCmd -> do
          handleCmdConnection buildsome conn executingCmd
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
    protect mvar act =
      (act >> putMVar mvar ()) `E.catch` errHandler
    errHandler e@E.SomeException{} = E.throwTo (ecThreadId ec) e

recordInput :: ExecutingCommand -> FilePath -> IO ()
recordInput ec path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ (ecInputs ec) $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith (\_new old -> old) path mstat

recordOutput :: ExecutingCommand -> FilePath -> IO ()
recordOutput ec path =
  atomicModifyIORef'_ (ecOutputs ec) $ S.insert path

inputIgnored :: FilePath -> Bool
inputIgnored path = "/dev" `isPrefixOf` path

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
    Protocol.SymLink target linkPath -> reportOutput linkPath >> reportInput target
    Protocol.Link src dest ->
      fail $ unwords ["Hard links not supported:", show src, "->", show dest]
      -- TODO: Record the fact it's a link
      --reportOutput dest >> reportInput src

    -- inputs
    Protocol.Open path Protocol.OpenReadMode _creationMode -> reportInput path
    Protocol.Access path _mode -> reportInput path
    Protocol.Stat path -> reportInput path
    Protocol.LStat path -> reportInput path
    Protocol.OpenDir path -> reportInput path
    Protocol.ReadLink path -> reportInput path
  where
    outputPaths = targetOutputPaths (ecTarget ec)
    reason = Protocol.showFunc msg ++ " done by " ++ show (ecCmd ec)
    isValidOutput path =
      path `elem` outputPaths || allowedUnspecifiedOutput path
    reportInput path
      | inputIgnored path = sendGo conn
      | otherwise = do
        -- Temporarily paused, so we can temporarily release parallelism
        -- semaphore
        withReleasedParallelism buildsome $ need buildsome reason [path]
        sendGo conn
        unless (isValidOutput path) $ recordInput ec path
    reportOutput fullPath = do
      path <- Dir.makeRelativeToCurrentDirectory fullPath
      unless (isValidOutput path) $ do
        fail $ concat [ show (ecCmd ec), " wrote to an unspecified output file: ", show path
                      , ", allowed outputs: ", show outputPaths ]
      recordOutput ec path

toBuildMaps :: [Target] -> BuildMaps
toBuildMaps targets = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, target)
      | target <- targets
      , outputPath <- targetOutputPaths target
      ]
    pair target = (minimum (targetOutputPaths target), target)
    childrenMap =
      M.fromListWith (++)
      [ (takeDirectory outputPath, [pair target])
      | (outputPath, target) <- outputs ]
    buildMap =
      M.fromListWith (error "Overlapping output paths")
      [ (outputPath, pair target)
      | (outputPath, target) <- outputs ]

withBuildsome ::
  Sophia.Db -> Int -> [Target] -> DeleteUnspecifiedOutputs ->
  FilePath -> (Buildsome -> IO a) -> IO a
withBuildsome db parallelism targets deleteUnspecifiedOutput ldPreloadPath body = do
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
        , bsBuildMaps = toBuildMaps targets
        , bsCurJobId = curJobId
        , bsDeleteUnspecifiedOutput = deleteUnspecifiedOutput
        , bsRestrictedParallelism = semaphore
        , bsDb = db
        }

  withUnixSeqPacketListener serverFilename $ \listener ->
    AsyncContext.new $ \ctx -> do
      _ <- AsyncContext.spawn ctx $ forever $ do
        (conn, _srcAddr) <- Sock.accept listener
        AsyncContext.spawn ctx $ serve buildsome conn
      body buildsome

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  progName <- getProgName
  Dir.canonicalizePath (takeDirectory progName </> "fs_override.so")

nextJobId :: Buildsome -> IO Int
nextJobId buildsome =
  atomicModifyIORef (bsCurJobId buildsome) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: Buildsome -> Reason -> [FilePath] -> IO ()
need buildsome reason paths = do
  slaves <- concat <$> mapM (makeSlaves buildsome reason) paths
  traverse_ slaveWait slaves

makeSlaves :: Buildsome -> Reason -> FilePath -> IO [Slave]
makeSlaves buildsome reason path = do
  mSlave <-
    traverse (mkTargetSlave reason) $ M.lookup path buildMap
  childSlaves <- traverse (mkTargetSlave (reason ++ "(Container directory)")) children
  return (maybeToList mSlave ++ childSlaves)
  where
    children = M.findWithDefault [] path childrenMap
    BuildMaps buildMap childrenMap = bsBuildMaps buildsome
    mkTargetSlave nuancedReason (outPathRep, target) =
      makeSlaveForRepPath buildsome nuancedReason outPathRep target

verifyOutputList :: Buildsome -> Set FilePath -> Target -> IO ()
verifyOutputList buildsome actualOutputs target = do
  unless (S.null unusedOutputs) $
    putStrLn $ "WARNING: Unused outputs: " ++ show (S.toList unusedOutputs)

  unspecifiedOutputs <- filterM fileExists $ S.toList (actualOutputs `S.difference` specifiedOutputs)
  case bsDeleteUnspecifiedOutput buildsome of
    DeleteUnspecifiedOutputs ->
      forM_ unspecifiedOutputs $
      \out -> do
        exists <- getMFileStatus out
        when (isJust exists) $ do
          putStrLn $ "Removing leaked/unspecified output: " ++ show out
          Dir.removeFile out
    DontDeleteUnspecifiedOutputs ->
      when (not (null unspecifiedOutputs)) $
      putStrLn $ concat
      [ "WARNING: Keeping leaked unspecified outputs: "
      , show unspecifiedOutputs ]
  where
    specifiedOutputs = S.fromList (targetOutputPaths target)
    unusedOutputs = specifiedOutputs `S.difference` actualOutputs

fileDescOfMStat :: FilePath -> Maybe FileStatus -> IO FileDesc
fileDescOfMStat path oldMStat = do
  mContentHash <-
    case oldMStat of
    Just stat | isRegularFile stat ->
      Just . MD5.hash <$>
      (BS.readFile path `catchDoesNotExist`
       fail (show path ++ " deleted during build!"))
    _ -> return Nothing
  -- Verify file did not change since we took its first mtime:
  newMStat <- getMFileStatus path
  when (not (compareMTimes oldMStat newMStat)) $ fail $
    show path ++ " changed during build!"
  case newMStat of
    Nothing -> return NoFile
    Just stat
      | isRegularFile stat ->
        return . RegularFile $
        fromMaybe (error ("File disappeared: " ++ show path))
        mContentHash
      | isDirectory stat -> return Directory
      | isSymbolicLink stat -> Symlink <$> readSymbolicLink path
      | otherwise -> fail $ "Unsupported file type: " ++ show path
  where
    compareMTimes x y =
      (modificationTime <$> x) ==
      (modificationTime <$> y)

getFileDesc :: FilePath -> IO FileDesc
getFileDesc path = fileDescOfMStat path =<< getMFileStatus path

targetKey :: Target -> ByteString
targetKey target =
  MD5.hash $ BS.pack (unlines (targetCmds target)) -- TODO: Canonicalize commands (whitespace/etc)

data ExecutionLog = ExecutionLog
  { _elInputsDescs :: Map FilePath FileDesc
  , _elOutputsDescs :: Map FilePath FileDesc
  } deriving (Generic, Show)
instance Binary ExecutionLog

setKey :: Binary a => Buildsome -> ByteString -> a -> IO ()
setKey buildsome key val = Sophia.setValue (bsDb buildsome) key $ runPut $ put val

getKey :: Binary a => Buildsome -> ByteString -> IO (Maybe a)
getKey buildsome key = fmap (runGet get) <$> Sophia.getValue (bsDb buildsome) key

saveExecutionLog :: Buildsome -> Target -> Map FilePath (Maybe FileStatus) -> Set FilePath -> IO ()
saveExecutionLog buildsome target inputsMStats actualOutputs = do
  inputsDescs <- M.traverseWithKey fileDescOfMStat inputsMStats
  outputDescPairs <-
    forM (S.toList actualOutputs) $ \outPath -> do
      fileDesc <- getFileDesc outPath
      return (outPath, fileDesc)
  let execLog = ExecutionLog inputsDescs (M.fromList outputDescPairs)
  setKey buildsome (targetKey target) execLog

-- TODO: Remember the order of input files' access so can iterate here
-- in order
tryApplyExecutionLog :: Buildsome -> Target -> IO Bool
tryApplyExecutionLog buildsome target = do
  mExecutionLog <- getKey buildsome (targetKey target)
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return False
    Just (ExecutionLog inputsDescs outputsDescs) -> do
      res <-
        runEitherT $ do
          -- TODO: This is good for parallelism, but bad if the set of
          -- inputs changed, as it may build stuff that's no longer
          -- required:
          let reason =
                "Previous Execution Log of " ++ show (targetOutputPaths target)
          liftIO $ do
            speculativeSlaves <- concat <$> mapM (makeSlaves buildsome reason) (M.keys inputsDescs)
            let hintReason = "Hint from " ++ show (take 1 (targetOutputPaths target))
            hintedSlaves <- concat <$> mapM (makeSlaves buildsome hintReason) (targetInputHints target)
            traverse_ slaveWait (speculativeSlaves ++ hintedSlaves)

          verifyNoChange "input" inputsDescs
          -- For now, we don't store the output files' content
          -- anywhere besides the actual output files, so just verify
          -- the output content is still correct
          verifyNoChange "output" outputsDescs
      case res of
        Left (str, filePath, _oldDesc, _newDesc) -> do
          putStrLn $ concat
            ["Execution log did not match because ", str, ": ", show filePath, " changed"]
          return False
        Right () -> do
          putStrLn $ "Execution log match for: " ++ show (targetOutputPaths target)
          return True
  where
    verifyNoChange str descs =
      forM_ (M.toList descs) $ \(filePath, oldDesc) -> do
        newDesc <- liftIO $ getFileDesc filePath
        when (oldDesc /= newDesc) $ left (str, filePath, oldDesc, newDesc) -- fail entire computation


makeSlaveForRepPath :: Buildsome -> Reason -> FilePath -> Target -> IO Slave
makeSlaveForRepPath buildsome reason outPathRep target = do
  newSlaveMVar <- newEmptyMVar
  E.mask $ \restoreMask -> do
    getSlave <-
      atomicModifyIORef (bsSlaveByRepPath buildsome) $
      \oldSlaveMap ->
      case M.lookup outPathRep oldSlaveMap of
      Nothing ->
        ( M.insert outPathRep newSlaveMVar oldSlaveMap
        , resultIntoMVar newSlaveMVar =<< spawnSlave restoreMask
        )
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
    getSlave
  where
    resultIntoMVar mvar x = putMVar mvar x >> return x
    spawnSlave restoreMask = do
      success <- tryApplyExecutionLog buildsome target
      if success
        then Slave target <$> async (return ())
        else do
          execution <- async . restoreMask $ do
            (inputs, outputs) <- withAllocatedParallelism buildsome $ do
              buildHinted buildsome target
              unzip <$> mapM (runCmd buildsome target reason) (targetCmds target)
            saveExecutionLog buildsome target (M.unions inputs) (S.unions outputs)
          return $ Slave target execution

buildHinted :: Buildsome -> Target -> IO ()
buildHinted buildsome target =
  need buildsome ("Hint from " ++ show (take 1 (targetOutputPaths target)))
  (targetInputHints target)

runCmd ::
  Buildsome -> Target -> String -> String ->
  IO (Map FilePath (Maybe FileStatus), Set FilePath)
runCmd buildsome target reason cmd = do
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef S.empty
  activeConnections <- newIORef []

  cmdIdNum <- nextJobId buildsome
  let cmdId = BS.pack ("cmd" ++ show cmdIdNum)
  tid <- myThreadId
  let ec = ExecutingCommand cmd tid target inputsRef outputsRef activeConnections
  putStrLn $ concat ["{ ", show cmd, ": ", reason]
  atomicModifyIORef_ (bsRunningCmds buildsome) $ M.insert cmdId ec
  shellCmdVerify ["HOME", "PATH"] (mkEnvVars buildsome cmdId) cmd
  putStrLn $ concat ["} ", show cmd]

  -- Give all connections a chance to complete and perhaps fail
  -- this execution:
  mapM_ readMVar =<< readIORef activeConnections

  actualOutputs <- readIORef outputsRef
  verifyOutputList buildsome actualOutputs target

  inputsMStats <- readIORef inputsRef
  return (inputsMStats, actualOutputs)

mkEnvVars :: Buildsome -> ByteString -> Process.Env
mkEnvVars buildsome cmdId =
    [ ("LD_PRELOAD", bsLdPreloadPath buildsome)
    , ("EFBUILD_MASTER_UNIX_SOCKADDR", bsAddress buildsome)
    , ("EFBUILD_CMD_ID", BS.unpack cmdId)
    ]

parseMakefile :: FilePath -> IO Makefile
parseMakefile makefileName = do
  parseResult <- P.parseOnly makefileParser <$> BS.readFile makefileName
  case parseResult of
    Left err -> fail $ "Makefile parse error: " ++ err
    Right makefile -> return makefile

withDb :: FilePath -> (Sophia.Db -> IO a) -> IO a
withDb dbFileName body = do
  Sophia.withEnv $ \env -> do
    Sophia.openDir env Sophia.ReadWrite Sophia.AllowCreation dbFileName
    Sophia.withDb env $ \db ->
      body db

main :: IO ()
main = do
  Opt makefileName mparallelism deleteUnspecifiedOutput <- getOpt
  let buildDbFilename = takeDirectory makefileName </> "build.db"
      parallelism = fromMaybe 1 mparallelism
  makefile <- parseMakefile makefileName
  withDb buildDbFilename $ \db -> do
    let targets = makefileTargets makefile
    ldPreloadPath <- getLdPreloadPath
    withBuildsome db parallelism targets deleteUnspecifiedOutput ldPreloadPath $
      \buildsome ->
      case targets of
      [] -> putStrLn "Empty makefile, done nothing..."
      (target:_) ->
        need buildsome "First target in Makefile" $
        take 1 (targetOutputPaths target)
