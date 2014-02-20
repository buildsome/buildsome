{-# OPTIONS -Wall -O2 #-}
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
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
import Lib.Process (getOutputs)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import System.Directory (canonicalizePath, makeRelativeToCurrentDirectory)
import System.Environment (getProgName)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import System.Process
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

type Reason = String
type SlaveId = BS.ByteString

data BuildStep = BuildStep
  { buildStepOutputs :: [FilePath]
  , buildStepCmd :: String
  }

data Slave = Slave
  { _slaveIdentifier :: SlaveId
  , slaveBuildStep :: BuildStep
  , slaveExecution :: Async ()
  }

data MasterServer = MasterServer
  { masterSlaveBySlaveId :: IORef (Map SlaveId (MVar Slave))
  , masterSlaveByRepPath :: IORef (Map FilePath (MVar Slave))
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  , masterBuildMap :: Map FilePath (FilePath, BuildStep) -- output paths -> min(representative) path and original spec
  , masterChildrenMap :: Map FilePath [(FilePath, BuildStep)] -- parent/dir paths -> all build steps that build directly into it
  , masterCurJobId :: IORef Int
  , masterSemaphore :: MSem Int -- ^ Restricts parallelism
  }

slaveWait :: Slave -> IO ()
slaveWait = wait . slaveExecution

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

-- | Opposite of MSem.with
localSemSignal :: MSem Int -> IO a -> IO a
localSemSignal sem = E.bracket_ (MSem.signal sem) (MSem.wait sem)

needAndGo :: MasterServer -> Reason -> String -> FilePath -> Socket -> IO ()
needAndGo masterServer reason _cmd path conn = do
  -- Temporarily paused, so we can temporarily release semaphore
  localSemSignal (masterSemaphore masterServer) $ do
--    putStrLn $ unwords ["-", show reason, show cmd]
    need masterServer reason [path]
--  putStrLn $ unwords ["+", show reason, show cmd]
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
    Just pidSlaveId -> do
      buildStepOfSlaveId <- readIORef (masterSlaveBySlaveId masterServer)
      case M.lookup slaveId buildStepOfSlaveId of
        Nothing -> do
          let slaveIds = M.keys buildStepOfSlaveId
          fail $ "Bad slave id: " ++ show slaveId ++ " mismatches all: " ++ show slaveIds
        Just slaveMVar -> do
          slave <- readMVar slaveMVar
          -- putStrLn $ concat
          --   [ "Got connection from ", BS.unpack slaveId
          --   , " (", show (buildStepCmd (slaveBuildStep slave)), show pid, ":", show tid, ")"
          --   ]
          handleSlaveConnection slave
            `E.catch` \e@E.SomeException{} -> cancelWith (slaveExecution slave) e
      where
        [_pidStr, _tidStr, slaveId] = BS.split ':' pidSlaveId
        -- getPid :: BS.ByteString -> Int
        -- getPid = read . BS.unpack
        -- pid = getPid pidStr
        -- tid = getPid tidStr
  where
    handleSlaveConnection slave =
      recvLoop_ 8192 (handleMsg slave . Protocol.parseMsg) conn
    handleMsg slave msg = do
      -- putStrLn $ "Got " ++ Protocol.showFunc msg
      let BuildStep outputPaths cmd = slaveBuildStep slave
          reason = Protocol.showFunc msg ++ " done by " ++ show cmd
          pauseToBuild path = needAndGo masterServer reason cmd path conn
          verifyLegalOutput fullPath = do
            path <- makeRelativeToCurrentDirectory fullPath
            when (path `notElem` outputPaths &&
                  not (allowedUnspecifiedOutput path)) $
              fail $ concat [ show cmd, " wrote to an unspecified output file: ", show path
                            , " (", show outputPaths, ")" ]
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

toBuildMap ::
  [BuildStep] ->
  ( Map FilePath (FilePath, BuildStep)
  , Map FilePath [(FilePath, BuildStep)]
  )
toBuildMap buildSteps = (buildMap, childrenMap)
  where
    outputs =
      [ (outputPath, buildStep)
      | buildStep <- buildSteps
      , outputPath <- buildStepOutputs buildStep
      ]
    pair buildStep = (minimum (buildStepOutputs buildStep), buildStep)
    childrenMap =
      M.fromListWith (++)
      [ (takeDirectory outputPath, [pair buildStep])
      | (outputPath, buildStep) <- outputs ]
    buildMap =
      M.fromListWith (error "Overlapping output paths")
      [ (outputPath, pair buildStep)
      | (outputPath, buildStep) <- outputs ]

withServer :: Int -> [BuildStep] -> FilePath -> (MasterServer -> IO a) -> IO a
withServer parallelCount buildSteps ldPreloadPath body = do
  buildStepOfSlaveId <- newIORef M.empty
  slaveMapByRepPath <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  curJobId <- newIORef 0
  semaphore <- MSem.new parallelCount
  let
    (buildMap, childrenMap) = toBuildMap buildSteps
    server =
      MasterServer
      { masterSlaveBySlaveId = buildStepOfSlaveId
      , masterSlaveByRepPath = slaveMapByRepPath
      , masterAddress = serverFilename
      , masterLdPreloadPath = ldPreloadPath
      , masterBuildMap = buildMap
      , masterChildrenMap = childrenMap
      , masterCurJobId = curJobId
      , masterSemaphore = semaphore
      }

  withUnixSeqPacketListener serverFilename $ \listener -> do
    connections <- newIORef M.empty
    let
      addConnection i connAsync = atomicModifyIORef_ connections $ M.insert i connAsync
      deleteConnection i = atomicModifyIORef_ connections $ M.delete i
      serverLoop = forM_ [(1 :: Int)..] $ \i -> do
        (conn, _srcAddr) <- Sock.accept listener
        -- TODO: This never frees the memory for each of the connection
        -- servers, even when they die. Best to maintain an explicit set
        -- of connections, and iterate to kill them when killing the
        -- server
        connAsync <-
          async $ serve server conn `E.finally` deleteConnection i
        addConnection i connAsync
      cancelConnections = traverse_ cancel =<< readIORef connections
    withAsync (serverLoop `E.finally` cancelConnections) $
      \_serverLoop -> body server

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  progName <- getProgName
  canonicalizePath (takeDirectory progName </> "fs_override.so")

exampleBuildSteps :: [BuildStep]
exampleBuildSteps =
  [ BuildStep ["example/a"] "gcc -o example/a example/a.o -g -Wall"
  , BuildStep ["example/a.o"] "gcc -c -o example/a.o example/a.c -g -Wall"
  , BuildStep ["example/auto.h"] "python example/generate.py"
  , BuildStep ["example/subdir.list"] "ls -l example/subdir > example/subdir.list"
  , BuildStep ["example/subdir/a"] "echo hi > example/subdir/a"
  ]

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> Reason -> [FilePath] -> IO ()
need masterServer reason paths = do
  slaves <- concat <$> mapM mkSlaves paths
  traverse_ slaveWait slaves
  where
    mkSlave nuancedReason = makeSlaveForRepPath masterServer nuancedReason
    mkSlaves path = do
      mSlave <-
        traverse (mkSlave reason) $
        M.lookup path (masterBuildMap masterServer)
      let children = M.findWithDefault [] path (masterChildrenMap masterServer)
      childSlaves <- traverse (mkSlave (reason ++ "(Container directory)")) children
      return (maybeToList mSlave ++ childSlaves)

makeSlaveForRepPath :: MasterServer -> Reason -> (FilePath, BuildStep) -> IO Slave
makeSlaveForRepPath masterServer reason (outPathRep, buildStep) = do
  jobId <- nextJobId masterServer
  let slaveId = BS.pack ("job" ++ show jobId)
  newSlaveMVar <- newEmptyMVar
  E.mask $ \restoreMask -> do
    getSlave <-
      atomicModifyIORef (masterSlaveByRepPath masterServer) $
      \oldSlaveMap ->
      case M.lookup outPathRep oldSlaveMap of
      Nothing -> (M.insert outPathRep newSlaveMVar oldSlaveMap, spawnSlave slaveId restoreMask newSlaveMVar)
      Just slaveMVar ->
        ( oldSlaveMap
        , do
            putStrLn $ concat [reason, ": Slave for ", outPathRep, "(", cmd, ") already spawned"]
            readMVar slaveMVar
        )
    getSlave
  where
    cmd = buildStepCmd buildStep
    showOutput name bs
      | BS.null bs = return ()
      | otherwise = do
        putStrLn (name ++ ":")
        BS.putStr bs

    spawnSlave slaveId restoreMask mvar = do
      atomicModifyIORef_ (masterSlaveBySlaveId masterServer) $ M.insert slaveId mvar
      execution <- async . restoreMask . MSem.with (masterSemaphore masterServer) $ do
        putStrLn $ concat ["{ ", show slaveId, " ", show cmd, ": ", reason]
        (exitCode, stdout, stderr) <- getOutputs (ShellCommand cmd) ["HOME", "PATH"] (envs slaveId)
        putStrLn $ concat ["} ", show slaveId, " ", show cmd]
        showOutput "STDOUT" stdout
        showOutput "STDERR" stderr
        case exitCode of
          ExitFailure {} -> fail $ concat [show cmd, " failed!"]
          _ -> return ()
      let slave = Slave slaveId buildStep execution
      putMVar mvar slave
      return slave
    envs slaveId =
        [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
        , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
        ]

main :: IO ()
main = do
  ldPreloadPath <- getLdPreloadPath
  withServer 1 exampleBuildSteps ldPreloadPath $ \masterServer -> do
    need masterServer "Top-level requested" ["example/a", "example/subdir.list"]
    threadDelay 100000
