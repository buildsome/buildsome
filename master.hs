{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Lib.Protocol (parseMsg)
import Lib.Sock (recvLoop, unixSeqPacketListener)
import Network.Socket (Socket)
import System.Environment (getEnv)
import System.Exit (ExitCode)
import System.Posix.Process (getProcessID)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.IO as IO

type SlaveId = BS.ByteString
type Pid = Int

data SlaveDesc = SlaveDesc (Chan (Pid, [BS.ByteString])) -- TODO: Outputs, etc.

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
          pid = read (BS.unpack pidStr)
      case M.lookup slaveId slavesMap of
        Nothing -> putStrLn $ "Bad slave id: " ++ show slaveId ++ " mismatches all: " ++ show (M.keys slavesMap)
        Just (SlaveDesc msgsVar) -> do
          putStrLn $ "Got connection from " ++ BS.unpack slaveId
          contents <- recvLoop 8192 conn
          writeChan msgsVar (pid, contents)

startServer :: IORef (Map SlaveId SlaveDesc) -> IO FilePath
startServer slavesMapRef = do
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  listener <- unixSeqPacketListener serverFilename
  _ <- forkIO $ forever $ do
    (conn, srcAddr) <- Sock.accept listener
    putStrLn $ "Connection from \"" ++ show srcAddr ++ "\""
    slavesMap <- readIORef slavesMapRef
    forkIO $ serve slavesMap conn
  return serverFilename

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
  putStrLn "STDOUT:"
  BS.putStr stdout
  putStrLn ""
  putStrLn "STDERR:"
  BS.putStr stderr
  putStrLn ""

main :: IO ()
main = do
  let slaveId = BS.pack "job1"

  slaveMsgsChan <- newChan
  slavesMapRef <- newIORef $ M.singleton slaveId (SlaveDesc slaveMsgsChan)

  serverFileName <- startServer slavesMapRef

  let cmd = "gcc -o example/a example/a.c -g -Wall"

      envs =
        [ ("LD_PRELOAD", "/home/peaker/Dropbox/Elastifile/build/prototype/fs_override.so")
        , ("EFBUILD_MASTER_UNIX_SOCKADDR", serverFileName)
        , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
        ]

  process <- makeProcess (ShellCommand cmd) envs
  waitProcess process

  putStrLn "Slave data:"
  slaveReader <- async $ forever $ do
    (slavePid, slaveMsgs) <- readChan slaveMsgsChan
    putStrLn $ "PID: " ++ show slavePid
    mapM_ (print . parseMsg) slaveMsgs

  threadDelay 100000

  cancel slaveReader
