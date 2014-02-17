{-# OPTIONS -Wall -O2 #-}
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Data.IORef
import Data.Map (Map)
import System.Environment (getEnv)
import System.Posix.Process (getProcessID)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Network.Socket (Socket, socket)
import qualified Network.Socket.ByteString as SockBS
import qualified Network.Socket as Sock
import qualified System.IO as IO

type SlaveId = BS.ByteString
type Pid = Int

data SlaveDesc = SlaveDesc (Chan (Pid, [BS.ByteString])) -- TODO: Outputs, etc.

unprefixed :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
unprefixed prefix full
  | prefix `BS.isPrefixOf` full = Just $ BS.drop (BS.length prefix) full
  | otherwise = Nothing

recvLoop :: Int -> Socket -> IO [BS.ByteString]
recvLoop maxFrameSize sock = do
  frame <- SockBS.recv sock maxFrameSize
  if BS.null frame then return []
    else do
      rest <- recvLoop maxFrameSize sock
      return (frame : rest)

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
        Just (SlaveDesc linesVar) -> do
          putStrLn $ "Got connection from " ++ BS.unpack slaveId
          contents <- recvLoop 8192 conn
          writeChan linesVar (pid, contents)

startServer :: IORef (Map SlaveId SlaveDesc) -> IO FilePath
startServer slavesMapRef = do
  masterPid <- getProcessID
  let serverFilename = "/tmp/efbuild-" ++ show masterPid
  serverSock <- socket Sock.AF_UNIX Sock.SeqPacket 0
  Sock.bind serverSock (Sock.SockAddrUnix serverFilename)
  Sock.listen serverSock 5
  _ <- forkIO $ forever $ do
    (conn, srcAddr) <- Sock.accept serverSock
    putStrLn $ "Connection from \"" ++ show srcAddr ++ "\""
    slavesMap <- readIORef slavesMapRef
    forkIO $ serve slavesMap conn
  return serverFilename

inheritedEnvs :: [String]
inheritedEnvs = ["HOME", "PATH"]

main :: IO ()
main = do
  oldEnvs <- forM inheritedEnvs $ \name -> do
    val <- getEnv name
    return (name, val)

  let slaveId = BS.pack "job1"

  slaveLinesChan <- newChan
  slavesMapRef <- newIORef $ M.singleton slaveId (SlaveDesc slaveLinesChan)

  serverFileName <- startServer slavesMapRef

  let cmd = "gcc -o example/a example/a.c -g -Wall"

  (Just stdinHandle, Just stdoutHandle, Just stderrHandle, process) <- createProcess CreateProcess
    { cwd = Nothing
    , cmdspec = ShellCommand cmd
    , env = Just $
      oldEnvs ++
      [ ("LD_PRELOAD", "/home/peaker/Dropbox/Elastifile/build/prototype/fs_override.so")
      , ("EFBUILD_MASTER_UNIX_SOCKADDR", serverFileName)
      , ("EFBUILD_SLAVE_ID", BS.unpack slaveId)
      ]
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

  stdout <- wait stdoutReader
  stderr <- wait stderrReader
  exitCode <- wait exitCodeReader

  putStrLn cmd
  putStrLn $ "ExitCode: " ++ show exitCode
  putStrLn "STDOUT:"
  BS.putStr stdout
  putStrLn ""
  putStrLn "STDERR:"
  BS.putStr stderr
  putStrLn ""

  putStrLn "Slave data:"
  slaveReader <- async $ forever $ do
    (slavePid, slaveLines) <- readChan slaveLinesChan
    putStrLn $ "PID: " ++ show slavePid
    mapM_ BS.putStrLn slaveLines
  threadDelay 100000
  cancel slaveReader
