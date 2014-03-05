module Lib.FSHook
  ( FSHook
  , with
  , AccessType(..), higherAccessType
  , AccessDoc
  , runCommand
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import Filesystem.Path.CurrentOS (encodeString)
import Lib.ByteString (unprefixed)
import Lib.IORef (atomicModifyIORef_)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Lib.StdOutputs (StdOutputs(..), printStdouts)
import Network.Socket (Socket)
import System.Argv0 (getArgv0)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import System.Process (CmdSpec(..))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.Process as Process
import qualified Lib.Protocol as Protocol
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Directory as Dir

type AccessDoc = String

type JobId = ByteString

data AccessType
  = AccessTypeFull -- open, stat, opendir, etc.  Depend on the content, and if directory, on the file listing
  | AccessTypeModeOnly -- access, readlink.  Depend on its existence/permission-modes only. If directory, does not depend on file listing
higherAccessType :: AccessType -> AccessType -> AccessType
higherAccessType AccessTypeModeOnly AccessTypeModeOnly = AccessTypeModeOnly
higherAccessType _ _ = AccessTypeFull

type InputHandler = AccessType -> AccessDoc -> FilePath -> IO ()
type OutputHandler = AccessDoc -> FilePath -> IO ()

data RunningJob = RunningJob
  { jobCmd :: String
  , jobActiveConnections :: IORef [MVar ()]
  , jobThreadId :: ThreadId
  , jobHandleInput :: InputHandler
  , jobHandleOutput :: OutputHandler
  }

data FSHook = FSHook
  { fsHookRunningJobs :: IORef (Map JobId RunningJob)
  , fsHookCurJobId :: IORef Int
  , fsHookLdPreloadPath :: FilePath
  , fsHookServerAddress :: FilePath
  }

nextJobId :: FSHook -> IO Int
nextJobId fsHook =
  atomicModifyIORef (fsHookCurJobId fsHook) $ \oldJobId -> (oldJobId+1, oldJobId)

serve :: FSHook -> Socket -> IO ()
serve fsHook conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> fail $ "Bad connection started with: " ++ show helloLine
    Just pidJobId -> do
      runningJobs <- readIORef (fsHookRunningJobs fsHook)
      case M.lookup jobId runningJobs of
        Nothing -> do
          let jobIds = M.keys runningJobs
          fail $ "Bad slave id: " ++ show jobId ++ " mismatches all: " ++ show jobIds
        Just job -> handleJobConnection fullTidStr conn job
      where
        fullTidStr = BS.unpack pidStr ++ ":" ++ BS.unpack tidStr
        [pidStr, tidStr, jobId] = BS.split ':' pidJobId

maxMsgSize :: Int
maxMsgSize = 8192

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  argv0 <- encodeString <$> getArgv0
  Dir.canonicalizePath (takeDirectory argv0 </> "fs_override.so")

with :: (FSHook -> IO a) -> IO a
with body = do
  ldPreloadPath <- getLdPreloadPath
  pid <- getProcessID
  curJobId <- newIORef 0
  let serverFilename = "/tmp/fshook-" ++ show pid
  withUnixSeqPacketListener serverFilename $ \listener -> do
    runningJobsRef <- newIORef M.empty
    let
      fsHook = FSHook
        { fsHookRunningJobs = runningJobsRef
        , fsHookCurJobId = curJobId
        , fsHookLdPreloadPath = ldPreloadPath
        , fsHookServerAddress = serverFilename
        }
    AsyncContext.new $ \ctx -> do
      _ <- AsyncContext.spawn ctx $ forever $ do
        (conn, _srcAddr) <- Sock.accept listener
        AsyncContext.spawn ctx $ serve fsHook conn
      body fsHook

sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS.pack "GO")

handleJobMsg :: String -> Socket -> RunningJob -> Protocol.Func -> IO ()
handleJobMsg _tidStr conn job msg = do
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
    Protocol.SymLink target linkPath -> reportOutput linkPath >> reportInput AccessTypeFull target
    Protocol.Link src dest -> forwardExceptions $
      fail $ unwords ["Hard links not supported:", show src, "->", show dest]
      -- TODO: Record the fact it's a link
      --reportOutput dest >> reportInput src

    -- inputs
    Protocol.Open path Protocol.OpenReadMode _creationMode -> reportInput AccessTypeFull path
    Protocol.Access path _mode -> reportInput AccessTypeModeOnly path
    Protocol.Stat path -> reportInput AccessTypeFull path
    Protocol.LStat path -> reportInput AccessTypeFull path
    Protocol.OpenDir path -> reportInput AccessTypeFull path
    Protocol.ReadLink path -> reportInput AccessTypeModeOnly path
  where
    actDesc = Protocol.showFunc msg ++ " done by " ++ show (jobCmd job)
    forwardExceptions =
      flip E.catch $ \e@E.SomeException {} -> E.throwTo (jobThreadId job) e
    reportInput accessType path =
      (`E.finally` sendGo conn) $
      forwardExceptions $ jobHandleInput job accessType actDesc path
    reportOutput path =
      forwardExceptions $ jobHandleOutput job actDesc path

handleJobConnection :: String -> Socket -> RunningJob -> IO ()
handleJobConnection tidStr conn job = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connFinishedMVar <- newEmptyMVar
  atomicModifyIORef_ (jobActiveConnections job) (connFinishedMVar:)
  protect connFinishedMVar $ do
    sendGo conn
    recvLoop_ maxMsgSize
      (handleJobMsg tidStr conn job . Protocol.parseMsg) conn
  where
    protect mvar act = act `E.finally` putMVar mvar ()

mkEnvVars :: FSHook -> JobId -> Process.Env
mkEnvVars fsHook jobId =
    [ ("LD_PRELOAD", fsHookLdPreloadPath fsHook)
    , ("EFBUILD_MASTER_UNIX_SOCKADDR", fsHookServerAddress fsHook)
    , ("EFBUILD_JOB_ID", BS.unpack jobId)
    ]

shellCmdVerify :: [String] -> Process.Env -> String -> IO StdOutputs
shellCmdVerify inheritEnvs newEnvs cmd = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand cmd) inheritEnvs newEnvs
  let stdouts = StdOutputs stdout stderr
  printStdouts stdouts
  case exitCode of
    ExitFailure {} -> do
      fail $ concat [show cmd, " failed!"]
    _ -> return stdouts

runCommand :: FSHook -> String -> InputHandler -> OutputHandler -> IO StdOutputs
runCommand fsHook cmd handleInput handleOutput = do
  activeConnections <- newIORef []
  jobIdNum <- nextJobId fsHook
  tid <- myThreadId

  let jobId = BS.pack ("cmd" ++ show jobIdNum)
      job = RunningJob
            { jobCmd = cmd
            , jobActiveConnections = activeConnections
            , jobThreadId = tid
            , jobHandleInput = handleInput
            , jobHandleOutput = handleOutput
            }
  atomicModifyIORef_ (fsHookRunningJobs fsHook) $ M.insert jobId job
  stdOutputs <- shellCmdVerify ["HOME", "PATH"] (mkEnvVars fsHook jobId) cmd
  -- Give all connections a chance to complete and perhaps fail
  -- this execution:
  mapM_ readMVar =<< readIORef activeConnections
  return stdOutputs
