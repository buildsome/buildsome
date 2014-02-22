{-# OPTIONS -Wall -O2 #-}
import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent.MSem (MSem)
import Control.Concurrent.MVar
import Control.Monad
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Lib.ByteString (unprefixed)
import Lib.IORef (atomicModifyIORef_)
import Lib.Makefile (Makefile(..), Target(..), makefileParser)
import Lib.Process (shellCmdVerify)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import System.Directory (canonicalizePath, makeRelativeToCurrentDirectory)
import System.Environment (getProgName, getArgs)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Database.Sophia as Sophia
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

type Reason = String
type CmdId = BS.ByteString

data Slave = Slave
  { slaveTarget :: Target
  , slaveExecution :: Async ()
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
  localSemSignal (masterSemaphore masterServer) $ do
--    putStrLn $ unwords ["-", show reason]
    need masterServer reason [path]
--  putStrLn $ unwords ["+", show reason]
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

handleSlaveMsg ::
  Show a =>
  MasterServer -> Socket -> a -> Slave -> Protocol.Func -> IO ()
handleSlaveMsg masterServer conn cmd slave msg =
  case msg of
  Protocol.Open path Protocol.OpenWriteMode _ -> verifyLegalOutput path
  Protocol.Open path _ (Protocol.Create _) -> verifyLegalOutput path
  Protocol.Creat path _ -> verifyLegalOutput path
  Protocol.Rename a b -> verifyLegalOutput a >> verifyLegalOutput b
  Protocol.Unlink path -> verifyLegalOutput path
  Protocol.Truncate path _ -> verifyLegalOutput path
  Protocol.Chmod path _ -> verifyLegalOutput path
  Protocol.Chown path _ _ -> verifyLegalOutput path
  Protocol.MkNod path _ _ -> verifyLegalOutput path -- TODO: Special mkNod handling?
  Protocol.MkDir path _ -> verifyLegalOutput path
  Protocol.RmDir path -> verifyLegalOutput path

  Protocol.SymLink target linkPath -> verifyLegalOutput linkPath >> pauseToBuild target
  Protocol.Link src dest -> verifyLegalOutput dest >> pauseToBuild src

  Protocol.Open path Protocol.OpenReadMode _creationMode -> pauseToBuild path
  Protocol.Access path _mode -> pauseToBuild path
  Protocol.Stat path -> pauseToBuild path
  Protocol.LStat path -> pauseToBuild path
  Protocol.OpenDir path -> pauseToBuild path
  Protocol.ReadLink path -> pauseToBuild path
  where
    outputPaths = targetOutputPaths (slaveTarget slave)
    reason = Protocol.showFunc msg ++ " done by " ++ show cmd
    pauseToBuild path = needAndGo masterServer reason path conn
    verifyLegalOutput fullPath = do
      path <- makeRelativeToCurrentDirectory fullPath
      when (path `notElem` outputPaths &&
            not (allowedUnspecifiedOutput path)) $ do
        fail $ concat [ show cmd, " wrote to an unspecified output file: ", show path
                      , " (", show outputPaths, ")" ]

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
withServer db parallelCount targets ldPreloadPath body = do
  runningCmds <- newIORef M.empty
  slaveMapByRepPath <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  curJobId <- newIORef 0
  semaphore <- MSem.new parallelCount
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
  canonicalizePath (takeDirectory progName </> "fs_override.so")

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> Reason -> [FilePath] -> IO ()
need masterServer reason paths = do
  slaves <- concat <$> mapM mkSlaves paths
  traverse_ slaveWait slaves
  where
    mkSlave nuancedReason (outPathRep, target) = do
      need masterServer ("Hint from: " ++ show outPathRep)
        (targetInputHints target)
      makeSlaveForRepPath masterServer nuancedReason outPathRep target
    mkSlaves path = do
      mSlave <-
        traverse (mkSlave reason) $
        M.lookup path (masterBuildMap masterServer)
      let children = M.findWithDefault [] path (masterChildrenMap masterServer)
      childSlaves <- traverse (mkSlave (reason ++ "(Container directory)")) children
      return (maybeToList mSlave ++ childSlaves)

makeSlaveForRepPath :: MasterServer -> Reason -> FilePath -> Target -> IO Slave
makeSlaveForRepPath masterServer reason outPathRep target = do
  newSlaveMVar <- newEmptyMVar
  E.mask $ \restoreMask -> do
    getSlave <-
      atomicModifyIORef (masterSlaveByRepPath masterServer) $
      \oldSlaveMap ->
      case M.lookup outPathRep oldSlaveMap of
      Nothing -> (M.insert outPathRep newSlaveMVar oldSlaveMap, spawnSlave restoreMask newSlaveMVar)
      Just slaveMVar ->
        ( oldSlaveMap
        , do
            putStrLn $ concat [reason, ": Slave for ", outPathRep, show (take 1 cmds), " already spawned"]
            readMVar slaveMVar
        )
    getSlave
  where
    cmds = targetCmds target
    spawnSlave restoreMask mvar = do
      activeConnections <- newIORef []
      execution <- async . restoreMask . MSem.with (masterSemaphore masterServer) $ do
        mapM_ (runCmd mvar) cmds
        -- Give all connections a chance to complete and perhaps fail
        -- this execution:
        mapM_ readMVar =<< readIORef activeConnections
      let slave = Slave target execution activeConnections
      putMVar mvar slave
      return slave
    runCmd mvar cmd = do
      cmdIdNum <- nextJobId masterServer
      let cmdId = BS.pack ("cmd" ++ show cmdIdNum)
      putStrLn $ concat ["{ ", show cmd, ": ", reason]
      atomicModifyIORef_ (masterRunningCmds masterServer) $ M.insert cmdId (cmd, mvar)
      shellCmdVerify ["HOME", "PATH"] (envs cmdId) cmd
      putStrLn $ concat ["} ", show cmd]
    envs cmdId =
        [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
        , ("EFBUILD_CMD_ID", BS.unpack cmdId)
        ]

parseCmdArgs :: IO (FilePath, FilePath)
parseCmdArgs = do
  progName <- getProgName
  args <- getArgs
  makefileName <-
    case args of
    [makefileName] -> return makefileName
    _ -> fail $ "Usage: " ++ progName ++ " <makefilepath>"
  return (makefileName, takeDirectory makefileName </> "build.db")

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
  (makefileName, buildDbFilename) <- parseCmdArgs
  makefile <- parseMakefile makefileName
  withDb buildDbFilename $ \db -> do
    let targets = makefileTargets makefile
    ldPreloadPath <- getLdPreloadPath
    withServer db 2 targets ldPreloadPath $ \masterServer ->
      case targets of
      [] -> putStrLn "Empty makefile, done nothing..."
      (target:_) ->
        need masterServer "First target in Makefile" $
        take 1 (targetOutputPaths target)
