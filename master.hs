{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Lib.Process (Process, makeProcess, waitProcess)
import Lib.Sock (recvLoop_, unixSeqPacketListener)
import Lib.ByteString (unprefixed)
import Network.Socket (Socket)
import System.Directory (canonicalizePath)
import System.Environment (getProgName)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

type SlaveId = BS.ByteString
type Pid = Int

type BuildMap = Map FilePath String

data Slave = Slave
  { _slaveCmd :: String
  , slaveProcess :: Process
  }

data MasterServer = MasterServer
  { masterSlaveMap :: IORef (Map SlaveId (MVar Slave))
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  , masterBuildMap :: BuildMap
  , masterCurJobId :: IORef Int
  }

slaveWait :: Slave -> IO ()
slaveWait = waitProcess . slaveProcess

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

buildGo :: MasterServer -> FilePath -> Socket -> IO ()
buildGo masterServer path conn = do
  need masterServer path
  sendGo conn

serve :: MasterServer -> Socket -> IO ()
serve masterServer conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> putStrLn $ "Bad connection started with: " ++ show helloLine
    Just pidSlaveId -> do
      slavesMap <- readIORef (masterSlaveMap masterServer)
      case M.lookup slaveId slavesMap of
        Nothing -> fail $ "Bad slave id: " ++ show slaveId ++ " mismatches all: " ++ show (M.keys slavesMap)
        Just _slaveMVar -> do
          -- slave <- readMVar slaveMVar
          putStrLn $ concat ["Got connection from " ++ BS.unpack slaveId ++ " (", show pid, ":", show tid, ")"]
          recvLoop_ 8192 (handleMsg . Protocol.parseMsg) conn
      where
        [pidStr, tidStr, slaveId] = BS.split ':' pidSlaveId
        getPid :: BS.ByteString -> Pid
        getPid = read . BS.unpack
        pid = getPid pidStr
        tid = getPid tidStr
  where
    handleMsg msg = do
      putStrLn $ "Got " ++ Protocol.showFunc msg
      case msg of
        Protocol.Open path Protocol.OpenReadMode _creationMode -> buildGo masterServer path conn
        Protocol.Access path _mode -> buildGo masterServer path conn
        _ -> return ()

startServer :: BuildMap -> FilePath -> IO MasterServer
startServer buildMap ldPreloadPath = do
  slavesMapRef <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  listener <- unixSeqPacketListener serverFilename
  curJobId <- newIORef 0
  let
    server =
      MasterServer
      { masterSlaveMap = slavesMapRef
      , masterAddress = serverFilename
      , masterLdPreloadPath = ldPreloadPath
      , masterBuildMap = buildMap
      , masterCurJobId = curJobId
      }
  _ <- forkIO $ forever $ do
    (conn, srcAddr) <- Sock.accept listener
    putStrLn $ "Connection from \"" ++ show srcAddr ++ "\""
    forkIO $ serve server conn
  return server

makeSlave :: MasterServer -> SlaveId -> String -> IO Slave
makeSlave masterServer slaveId cmd = do
  newSlaveMVar <- newEmptyMVar
  getSlave <- atomicModifyIORef (masterSlaveMap masterServer) $
    \oldSlaveMap ->
    case M.lookup slaveId oldSlaveMap of
    Nothing -> (M.insert slaveId newSlaveMVar oldSlaveMap, spawnSlave newSlaveMVar)
    Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
  getSlave
  where
    spawnSlave mvar = do
      process <- makeProcess (ShellCommand cmd) ["HOME", "PATH"] envs
      let slave = Slave cmd process
      putMVar mvar slave
      return slave
    envs =
        [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
        , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
        ]

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  progName <- getProgName
  canonicalizePath (takeDirectory progName </> "fs_override.so")

exampleBuildMap :: BuildMap
exampleBuildMap = M.fromList
  [ ("example/a", "gcc -o example/a example/a.o -g -Wall")
  , ("example/a.o", "gcc -o example/a.o example/a.c -g -Wall")
  ]

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> FilePath -> IO ()
need masterServer path = do
  putStrLn $ "Need: " ++ show path
  case M.lookup path (masterBuildMap masterServer) of
    Nothing -> return ()
    Just cmd -> do
      jobId <- nextJobId masterServer
      let slaveId = BS.pack ("job" ++ show jobId)
      slave <- makeSlave masterServer slaveId cmd
      slaveWait slave

main :: IO ()
main = do
  masterServer <- startServer exampleBuildMap =<< getLdPreloadPath
  need masterServer "example/a"
  threadDelay 100000
