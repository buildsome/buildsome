{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Lib.Sock (recvLoop_, unixSeqPacketListener)
import Network.Socket (Socket)
import System.Directory (canonicalizePath)
import System.Environment (getEnv, getProgName)
import System.Exit (ExitCode)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.IO as IO

type SlaveId = BS.ByteString
type Pid = Int

data SlaveDesc = SlaveDesc -- TODO: Outputs, etc.

unprefixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing

serve :: Map SlaveId SlaveDesc -> Socket -> IO ()
serve slavesMap conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> putStrLn $ "Bad connection started with: " ++ show helloLine
    Just pidSlaveId -> do
      let [pidStr, slaveId] = BS.split ':' pidSlaveId
          pid :: Pid
          pid = read (BS.unpack pidStr)
      case M.lookup slaveId slavesMap of
        Nothing -> putStrLn $ "Bad slave id: " ++ show slaveId ++ " mismatches all: " ++ show (M.keys slavesMap)
        Just SlaveDesc -> do
          putStrLn $ concat ["Got connection from " ++ BS.unpack slaveId ++ " (", show pid, ")"]
          recvLoop_ 8192 (handleMsg . Protocol.parseMsg) conn
  where
    handleMsg msg = do
      putStrLn $ "Got " ++ Protocol.showFunc msg
      case msg of
        Protocol.Open _path Protocol.OpenReadMode _creationMode ->
          void $ SockBS.send conn (BS.pack "GO")
        _ -> return ()

data MasterServer = MasterServer
  { masterSlaveMap :: IORef (Map SlaveId SlaveDesc)
  , masterAddress :: FilePath -- unix socket server
  , masterLdPreloadPath :: FilePath
  }

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ioref f = atomicModifyIORef ioref (\x -> (f x, ()))

masterAddSlave :: MasterServer -> SlaveId -> IO ()
masterAddSlave masterServer slaveId = do
  atomicModifyIORef_ (masterSlaveMap masterServer) $
    M.insert slaveId SlaveDesc

startServer :: FilePath -> IO MasterServer
startServer ldPreloadPath = do
  slavesMapRef <- newIORef M.empty
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  listener <- unixSeqPacketListener serverFilename
  _ <- forkIO $ forever $ do
    (conn, srcAddr) <- Sock.accept listener
    putStrLn $ "Connection from \"" ++ show srcAddr ++ "\""
    slavesMap <- readIORef slavesMapRef
    forkIO $ serve slavesMap conn
  return $ MasterServer
    { masterSlaveMap = slavesMapRef
    , masterAddress = serverFilename
    , masterLdPreloadPath = ldPreloadPath
    }

inheritedEnvs :: [String]
inheritedEnvs = ["HOME", "PATH"]

type Env = [(String, String)]

data Process = Process
  { _processCmd :: CmdSpec
  , _stdoutReader :: Async BS.ByteString
  , _stderrReader :: Async BS.ByteString
  , _exitCodeReader :: Async ExitCode
  }

makeProcess :: CmdSpec -> Env -> IO Process
makeProcess cmd envs = do
  oldEnvs <- forM inheritedEnvs $ \name -> do
    val <- getEnv name
    return (name, val)
  (Just stdinHandle, Just stdoutHandle, Just stderrHandle, process) <- createProcess CreateProcess
    { cwd = Nothing
    , cmdspec = cmd
    , env = Just (oldEnvs ++ envs)
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = False
    , create_group = True
--    , delegate_ctlc = True
    }
  IO.hClose stdinHandle

  stdoutReader <- async (BS.hGetContents stdoutHandle)
  stderrReader <- async (BS.hGetContents stderrHandle)
  exitCodeReader <- async (waitForProcess process)

  return $ Process cmd stdoutReader stderrReader exitCodeReader

waitProcess :: Process -> IO ()
waitProcess (Process _cmd stdoutReader stderrReader exitCodeReader) = do
  stdout <- wait stdoutReader
  stderr <- wait stderrReader
  exitCode <- wait exitCodeReader

  putStrLn $ "ExitCode: " ++ show exitCode
  when (not (BS.null stdout)) $ do
    putStrLn "STDOUT:"
    BS.putStr stdout
    putStrLn ""
  when (not (BS.null stderr)) $ do
    putStrLn "STDERR:"
    BS.putStr stderr
    putStrLn ""

data Slave = Slave
  { _slaveIdentifier :: SlaveId
  , _slaveCmd :: String
  , slaveProcess :: Process
  }

makeSlave :: MasterServer -> SlaveId -> String -> IO Slave
makeSlave masterServer slaveId cmd = do
  masterAddSlave masterServer slaveId
  process <- makeProcess (ShellCommand cmd) envs
  return (Slave slaveId cmd process)
  where
    envs =
        [ ("LD_PRELOAD", masterLdPreloadPath masterServer)
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", masterAddress masterServer)
        , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
        ]

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  progName <- getProgName
  canonicalizePath (takeDirectory progName </> "fs_override.so")

main :: IO ()
main = do
  masterServer <- startServer =<< getLdPreloadPath
  slave <- makeSlave masterServer (BS.pack "job1") "gcc -o example/a example/a.c -g -Wall"
  waitProcess $ slaveProcess slave
  threadDelay 100000
