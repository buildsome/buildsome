{-# OPTIONS -Wall -O2 #-}
import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Lib.ByteString (unprefixed)
import Lib.IORef (atomicModifyIORef_, atomicModifyIORef'_)
import Lib.Makefile (Makefile(..), Target(..), makefileParser)
import Lib.Process (shellCmdVerify)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import Opts (getOpt, Opt(..))
import System.Environment (getProgName)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error
import System.Posix.Files (FileStatus, getFileStatus, isRegularFile, modificationTime)
import System.Posix.Process (getProcessID)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Database.Sophia as Sophia
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.Process as Process
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Directory as Dir

type Reason = String
type CmdId = ByteString

data Slave = Slave
  { slaveTarget :: Target
  , slaveExecution :: Async ()
  , -- For each slave input, record modification time before input is
    -- used, to compare it after slave is done
    slaveInputs :: IORef (Map FilePath (Maybe FileStatus))
  , slaveActiveConnections :: IORef [MVar ()]
  }

data MasterServer = MasterServer
  { masterRunningCmds :: IORef (Map CmdId (String, MVar Slave))
  , masterSlaveByRepPath :: IORef (Map FilePath (MVar Slave))
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  , masterBuildMap :: Map FilePath (FilePath, Target) -- output paths -> min(representative) path and original spec
  , masterChildrenMap :: Map FilePath [(FilePath, Target)] -- parent/dir paths -> all build steps that build directly into it
  , masterCurJobId :: IORef Int
  , masterSemaphore :: MSem Int -- ^ Restricts parallelism
  , masterDb :: Sophia.Db
  }

slaveWait :: Slave -> IO ()
slaveWait = wait . slaveExecution

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

-- | Opposite of MSem.with
localSemSignal :: MSem Int -> IO a -> IO a
localSemSignal sem = E.bracket_ (MSem.signal sem) (MSem.wait sem)

needAndGo :: MasterServer -> Reason -> FilePath -> Socket -> IO ()
needAndGo masterServer reason path conn = do
  -- Temporarily paused, so we can temporarily release semaphore
  localSemSignal (masterSemaphore masterServer) $
    need masterServer reason [path]
  sendGo conn

allowedUnspecifiedOutput :: FilePath -> Bool
allowedUnspecifiedOutput path = or
  [ "/tmp" `isPrefixOf` path
  , ".pyc" `isSuffixOf` path
  ]

serve :: MasterServer -> Socket -> IO ()
serve masterServer conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> fail $ "Bad connection started with: " ++ show helloLine
    Just pidCmdId -> do
      runningCmds <- readIORef (masterRunningCmds masterServer)
      case M.lookup cmdId runningCmds of
        Nothing -> do
          let cmdIds = M.keys runningCmds
          fail $ "Bad slave id: " ++ show cmdId ++ " mismatches all: " ++ show cmdIds
        Just (cmd, slaveMVar) -> do
          slave <- readMVar slaveMVar
          handleSlaveConnection masterServer conn cmd slave
      where
        [_pidStr, _tidStr, cmdId] = BS.split ':' pidCmdId

maxMsgSize :: Int
maxMsgSize = 8192

handleSlaveConnection :: Show a => MasterServer -> Socket -> a -> Slave -> IO ()
handleSlaveConnection masterServer conn cmd slave = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connFinishedMVar <- newEmptyMVar
  atomicModifyIORef_ (slaveActiveConnections slave) (connFinishedMVar:)
  protect connFinishedMVar $ do
    sendGo conn
    recvLoop_ maxMsgSize
      (handleSlaveMsg masterServer conn cmd slave .
       Protocol.parseMsg) conn
  where
    protect mvar act =
      (act >> putMVar mvar ()) `E.catch` errHandler
    errHandler e@E.SomeException{} = cancelWith (slaveExecution slave) e

catchDoesNotExist :: IO a -> IO a -> IO a
catchDoesNotExist act handler =
  E.catchJust predicate act $ \() -> handler
  where
    predicate e
      | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
      | otherwise = Nothing

getMFileStatus :: FilePath -> IO (Maybe FileStatus)
getMFileStatus path =
  (Just <$> getFileStatus path)
  `catchDoesNotExist` return Nothing

recordInput :: Slave -> FilePath -> IO ()
recordInput slave path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ (slaveInputs slave) $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith (\_new old -> old) path mstat

handleSlaveMsg ::
  Show a =>
  MasterServer -> Socket -> a -> Slave -> Protocol.Func -> IO ()
handleSlaveMsg masterServer conn cmd slave msg =
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
    Protocol.Link src dest -> reportOutput dest >> reportInput src

    -- inputs
    Protocol.Open path Protocol.OpenReadMode _creationMode -> reportInput path
    Protocol.Access path _mode -> reportInput path
    Protocol.Stat path -> reportInput path
    Protocol.LStat path -> reportInput path
    Protocol.OpenDir path -> reportInput path
    Protocol.ReadLink path -> reportInput path
  where
    outputPaths = targetOutputPaths (slaveTarget slave)
    reason = Protocol.showFunc msg ++ " done by " ++ show cmd
    isValidOutput path =
      path `elem` outputPaths || allowedUnspecifiedOutput path
    reportInput path = do
      needAndGo masterServer reason path conn
      unless (isValidOutput path) $ recordInput slave path
    reportOutput fullPath = do
      path <- Dir.makeRelativeToCurrentDirectory fullPath
      unless (isValidOutput path) $ do
        fail $ concat [ show cmd, " wrote to an unspecified output file: ", show path
                      , ", allowed outputs: ", show outputPaths ]
      -- TODO: recordOutput too so we can warn about unused specified outputs

toBuildMap ::
  [Target] ->
  ( Map FilePath (FilePath, Target)
  , Map FilePath [(FilePath, Target)]
  )
toBuildMap targets = (buildMap, childrenMap)
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

withServer :: Sophia.Db -> Int -> [Target] -> FilePath -> (MasterServer -> IO a) -> IO a
withServer db parallelism targets ldPreloadPath body = do
  runningCmds <- newIORef M.empty
  slaveMapByRepPath <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  curJobId <- newIORef 0
  semaphore <- MSem.new parallelism
  let masterServer =
        MasterServer
        { masterRunningCmds = runningCmds
        , masterSlaveByRepPath = slaveMapByRepPath
        , masterAddress = serverFilename
        , masterLdPreloadPath = ldPreloadPath
        , masterBuildMap = buildMap
        , masterChildrenMap = childrenMap
        , masterCurJobId = curJobId
        , masterSemaphore = semaphore
        , masterDb = db
        }

  withUnixSeqPacketListener serverFilename $ \listener ->
    AsyncContext.new $ \ctx -> do
      _ <- AsyncContext.spawn ctx $ forever $ do
        (conn, _srcAddr) <- Sock.accept listener
        AsyncContext.spawn ctx $ serve masterServer conn
      body masterServer
  where
    (buildMap, childrenMap) = toBuildMap targets

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  progName <- getProgName
  Dir.canonicalizePath (takeDirectory progName </> "fs_override.so")

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> Reason -> [FilePath] -> IO ()
need masterServer reason paths = do
  slaves <- concat <$> mapM (makeSlaves masterServer reason) paths
  traverse_ slaveWait slaves

makeSlaves :: MasterServer -> Reason -> FilePath -> IO [Slave]
makeSlaves masterServer reason path = do
  mSlaves <-
    traverse (mkTargetSlaves reason) $
    M.lookup path (masterBuildMap masterServer)
  let children = M.findWithDefault [] path (masterChildrenMap masterServer)
  childSlaves <- concat <$> traverse (mkTargetSlaves (reason ++ "(Container directory)")) children
  return (fromMaybe [] mSlaves ++ childSlaves)
  where
    mkTargetSlaves nuancedReason (outPathRep, target) = do
      slaves <-
        concat <$>
        mapM (makeSlaves masterServer ("Hint from: " ++ show outPathRep))
        (targetInputHints target)
      slave <- makeSlaveForRepPath masterServer nuancedReason outPathRep target
      return $ slave : slaves

makeSlaveForRepPath :: MasterServer -> Reason -> FilePath -> Target -> IO Slave
makeSlaveForRepPath masterServer reason outPathRep target = do
  newSlaveMVar <- newEmptyMVar
  E.mask $ \restoreMask -> do
    getSlave <-
      atomicModifyIORef (masterSlaveByRepPath masterServer) $
      \oldSlaveMap ->
      case M.lookup outPathRep oldSlaveMap of
      Nothing -> (M.insert outPathRep newSlaveMVar oldSlaveMap, spawnSlave restoreMask newSlaveMVar)
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
    getSlave
  where
    spawnSlave restoreMask mvar = do
      activeConnections <- newIORef []
      slaveInputsRef <- newIORef M.empty
      execution <- async . restoreMask $ do
        MSem.with (masterSemaphore masterServer) $ do
          mapM_ (runCmd masterServer reason mvar) $ targetCmds target
          -- Give all connections a chance to complete and perhaps fail
          -- this execution:
        mapM_ readMVar =<< readIORef activeConnections
        inputsMStats <- readIORef slaveInputsRef
        _contentHashes <- M.traverseWithKey summarizeInput inputsMStats
        -- TODO: Save these in db!
        return ()
      let slave = Slave target execution slaveInputsRef activeConnections
      putMVar mvar slave
      return slave

summarizeInput :: FilePath -> Maybe FileStatus -> IO (Maybe ByteString)
summarizeInput path oldMStat = do
  mContentHash <-
    case oldMStat of
    Just stat | isRegularFile stat ->
      (Just . MD5.hash <$> BS.readFile path)
      `catchDoesNotExist` fail (show path ++ " deleted during build!")
    _ -> return Nothing
  -- Verify file did not change since we took its first mtime:
  newMStat <- getMFileStatus path
  when (not (compareMTimes oldMStat newMStat)) $ fail $
    show path ++ " changed during build!"
  return $ mContentHash
  where
    compareMTimes x y =
      (modificationTime <$> x) ==
      (modificationTime <$> y)

runCmd :: MasterServer -> [Char] -> MVar Slave -> String -> IO ()
runCmd masterServer reason mvar cmd = do
  cmdIdNum <- nextJobId masterServer
  let cmdId = BS.pack ("cmd" ++ show cmdIdNum)
  putStrLn $ concat ["{ ", show cmd, ": ", reason]
  atomicModifyIORef_ (masterRunningCmds masterServer) $ M.insert cmdId (cmd, mvar)
  shellCmdVerify ["HOME", "PATH"] (mkEnvVars masterServer cmdId) cmd
  putStrLn $ concat ["} ", show cmd]

mkEnvVars :: MasterServer -> ByteString -> Process.Env
mkEnvVars masterServer cmdId =
    [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
    , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
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
  Opt makefileName mparallelism <- getOpt
  let buildDbFilename = takeDirectory makefileName </> "build.db"
      parallelism = fromMaybe 1 mparallelism
  makefile <- parseMakefile makefileName
  withDb buildDbFilename $ \db -> do
    let targets = makefileTargets makefile
    ldPreloadPath <- getLdPreloadPath
    withServer db parallelism targets ldPreloadPath $ \masterServer ->
      case targets of
      [] -> putStrLn "Empty makefile, done nothing..."
      (target:_) ->
        need masterServer "First target in Makefile" $
        take 1 (targetOutputPaths target)
