{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map (Map)
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
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

type SlaveId = BS.ByteString

data BuildStep = BuildStep
  { _buildStepOutputs :: [FilePath]
  , buildStepCmd :: String
  }

data Slave = Slave
  { _slaveId :: SlaveId
  , slaveBuildStep :: BuildStep
  , slaveExecution :: Async ()
  }

data MasterServer = MasterServer
  { masterSlaveBySlaveId :: IORef (Map SlaveId (MVar Slave))
  , masterSlaveByRepPath :: IORef (Map FilePath (MVar Slave))
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  , masterBuildMap :: Map FilePath (FilePath, BuildStep) -- output paths -> min(representative) path and original spec
  , masterCurJobId :: IORef Int
  }

slaveWait :: Slave -> IO ()
slaveWait = wait . slaveExecution

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

buildGo :: MasterServer -> FilePath -> Socket -> IO ()
buildGo masterServer path conn = do
  need masterServer path
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
          pauseToBuild path = buildGo masterServer path conn
          verifyLegalOutput fullPath = do
            path <- makeRelativeToCurrentDirectory fullPath
            when (path `notElem` outputPaths &&
                  not (allowedUnspecifiedOutput path)) $
              fail $ concat [ show cmd, " wrote to an unspecified output file: ", show path
                            , " (", show outputPaths, ")" ]
      case msg of
        Protocol.Open path Protocol.OpenWriteMode _ -> verifyLegalOutput path
        Protocol.Open path _ (Protocol.Create _) -> verifyLegalOutput path
        Protocol.Open path Protocol.OpenReadMode _creationMode -> pauseToBuild path
        Protocol.Access path _mode -> pauseToBuild path
        _ -> return ()

toBuildMap :: [BuildStep] -> Map FilePath (FilePath, BuildStep)
toBuildMap buildSteps =
  M.fromListWith (error "Overlapping output paths")
  [ (outputPath, (minimum outputPaths, buildStep))
  | buildStep@(BuildStep outputPaths _cmd) <- buildSteps
  , outputPath <- outputPaths
  ]

withServer :: [BuildStep] -> FilePath -> (MasterServer -> IO a) -> IO a
withServer buildSteps ldPreloadPath body = do
  buildStepOfSlaveId <- newIORef M.empty
  slaveMapByRepPath <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  curJobId <- newIORef 0
  let
    server =
      MasterServer
      { masterSlaveBySlaveId = buildStepOfSlaveId
      , masterSlaveByRepPath = slaveMapByRepPath
      , masterAddress = serverFilename
      , masterLdPreloadPath = ldPreloadPath
      , masterBuildMap = toBuildMap buildSteps
      , masterCurJobId = curJobId
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
  ]

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> FilePath -> IO ()
need masterServer path = do
  case M.lookup path (masterBuildMap masterServer) of
    Nothing -> return ()
    Just (repPath, buildStep) -> do
      jobId <- nextJobId masterServer
      let slaveId = BS.pack ("job" ++ show jobId)
      slave <- makeSlaveForRepPath masterServer slaveId repPath buildStep
      slaveWait slave

makeSlaveForRepPath :: MasterServer -> SlaveId -> FilePath -> BuildStep -> IO Slave
makeSlaveForRepPath masterServer slaveId outPath buildStep = do
  newSlaveMVar <- newEmptyMVar
  E.mask $ \restore -> do
    getSlave <-
      atomicModifyIORef (masterSlaveByRepPath masterServer) $
      \oldSlaveMap ->
      case M.lookup outPath oldSlaveMap of
      Nothing -> (M.insert outPath newSlaveMVar oldSlaveMap, spawnSlave restore newSlaveMVar)
      Just slaveMVar ->
        ( oldSlaveMap
        , putStrLn ("Slave for " ++ outPath ++ "(" ++ cmd ++ ") already spawned") >> readMVar slaveMVar
        )
    getSlave
  where
    cmd = buildStepCmd buildStep
    showOutput name bs
      | BS.null bs = return ()
      | otherwise = do
        putStrLn (name ++ ":")
        BS.putStr bs

    spawnSlave restore mvar = do
      putStrLn $ unwords [show cmd, "spawning as", show slaveId]
      atomicModifyIORef_ (masterSlaveBySlaveId masterServer) $ M.insert slaveId mvar
      execution <- async . restore $ do
        (exitCode, stdout, stderr) <- getOutputs (ShellCommand cmd) ["HOME", "PATH"] envs
        putStrLn $ concat [show cmd, " completed"]
        showOutput "STDOUT" stdout
        showOutput "STDERR" stderr
        case exitCode of
          ExitFailure {} -> fail $ concat [show cmd, " failed!"]
          _ -> return ()
      let slave = Slave slaveId buildStep execution
      putMVar mvar slave
      return slave
    envs =
        [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
        , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
        ]

main :: IO ()
main = do
  ldPreloadPath <- getLdPreloadPath
  withServer exampleBuildSteps ldPreloadPath $ \masterServer -> do
    need masterServer "example/a"
    threadDelay 100000
