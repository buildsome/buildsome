{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import Lib.ByteString (unprefixed)
import Lib.IORef (atomicModifyIORef_)
import Lib.Process (Process, makeProcess, waitProcess)
import Lib.Sock (recvLoop_, unixSeqPacketListener)
import Network.Socket (Socket)
import System.Directory (canonicalizePath)
import System.Environment (getProgName)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

type SlaveId = BS.ByteString
type Pid = Int

data Slave = Slave
  { _slaveId :: SlaveId
  , _slaveCmd :: String
  , slaveProcess :: Process
  }

data BuildStep = BuildStep
  { _buildStepOutputs :: [FilePath]
  , _buildStepCmd :: String
  }

data MasterServer = MasterServer
  { masterSlaveIds :: IORef (Set SlaveId)
  , masterSlaveByRepPath :: IORef (Map FilePath (MVar Slave))
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  , masterBuildMap :: Map FilePath (FilePath, BuildStep) -- output paths -> min(representative) path and original spec
  , masterCurJobId :: IORef Int
  }

slaveWait :: Slave -> IO ()
slaveWait = waitProcess . slaveProcess

sendGo :: Socket -> IO ()
sendGo conn = do
  putStrLn "Sending go"
  void $ SockBS.send conn (BS.pack "GO")

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
      slaveIds <- readIORef (masterSlaveIds masterServer)
      if slaveId `S.member` slaveIds then do
        -- slave <- readMVar slaveMVar
        putStrLn $ concat ["Got connection from " ++ BS.unpack slaveId ++ " (", show pid, ":", show tid, ")"]
        recvLoop_ 8192 (handleMsg . Protocol.parseMsg) conn
        else
        fail $ "Bad slave id: " ++ show slaveId ++ " mismatches all: " ++ show slaveIds
      where
        [pidStr, tidStr, slaveId] = BS.split ':' pidSlaveId
        getPid :: BS.ByteString -> Pid
        getPid = read . BS.unpack
        pid = getPid pidStr
        tid = getPid tidStr
  where
    handleMsg msg = do
      putStrLn $ "Got " ++ Protocol.showFunc msg
      let pauseToBuild path = buildGo masterServer path conn
      case msg of
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

startServer :: [BuildStep] -> FilePath -> IO MasterServer
startServer buildSteps ldPreloadPath = do
  slaveIdsRef <- newIORef S.empty
  slaveMapByRepPath <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  listener <- unixSeqPacketListener serverFilename
  curJobId <- newIORef 0
  let
    server =
      MasterServer
      { masterSlaveIds = slaveIdsRef
      , masterSlaveByRepPath = slaveMapByRepPath
      , masterAddress = serverFilename
      , masterLdPreloadPath = ldPreloadPath
      , masterBuildMap = toBuildMap buildSteps
      , masterCurJobId = curJobId
      }
  _ <- forkIO $ forever $ do
    (conn, srcAddr) <- Sock.accept listener
    putStrLn $ "Connection from \"" ++ show srcAddr ++ "\""
    forkIO $ serve server conn
  return server

makeSlaveForRepPath :: MasterServer -> SlaveId -> FilePath -> String -> IO Slave
makeSlaveForRepPath masterServer slaveId outPath cmd = do
  newSlaveMVar <- newEmptyMVar
  getSlave <-
    atomicModifyIORef (masterSlaveByRepPath masterServer) $
    \oldSlaveMap ->
    case M.lookup outPath oldSlaveMap of
    Nothing -> (M.insert outPath newSlaveMVar oldSlaveMap, spawnSlave newSlaveMVar)
    Just slaveMVar ->
      ( oldSlaveMap
      , putStrLn ("Slave for " ++ outPath ++ "(" ++ cmd ++ ") already running") >> readMVar slaveMVar
      )
  getSlave
  where
    spawnSlave mvar = do
      putStrLn $ unwords ["Spawning slave ", show slaveId, show cmd]
      atomicModifyIORef_ (masterSlaveIds masterServer) $ S.insert slaveId
      process <- makeProcess (ShellCommand cmd) ["HOME", "PATH"] envs
      let slave = Slave slaveId cmd process
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

exampleBuildSteps :: [BuildStep]
exampleBuildSteps =
  [ BuildStep ["example/a"] "gcc -o example/a example/a.o -g -Wall"
  , BuildStep ["example/a.o"] "gcc -c -o example/a.o example/a.c -g -Wall"
  ]

nextJobId :: MasterServer -> IO Int
nextJobId masterServer =
  atomicModifyIORef (masterCurJobId masterServer) $ \oldJobId -> (oldJobId+1, oldJobId)

need :: MasterServer -> FilePath -> IO ()
need masterServer path = do
  case M.lookup path (masterBuildMap masterServer) of
    Nothing -> return ()
    Just (repPath, BuildStep _paths cmd) -> do
      jobId <- nextJobId masterServer
      let slaveId = BS.pack ("job" ++ show jobId)
      slave <- makeSlaveForRepPath masterServer slaveId repPath cmd
      slaveWait slave

main :: IO ()
main = do
  masterServer <- startServer exampleBuildSteps =<< getLdPreloadPath
  need masterServer "example/a"
  threadDelay 100000
