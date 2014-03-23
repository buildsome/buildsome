{-# LANGUAGE DeriveDataTypeable #-}
module Lib.FSHook
  ( FSHook
  , with
  , Handlers(..)
  , AccessDoc
  , runCommand
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Typeable (Typeable)
import Lib.AccessType (AccessType(..))
import Lib.Argv0 (getArgv0)
import Lib.ByteString (unprefixed)
import Lib.IORef (atomicModifyIORef_)
import Lib.Sock (recvLoop_, withUnixSeqPacketListener)
import Network.Socket (Socket)
import Paths_buildsome (getDataFileName)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Lib.AsyncContext as AsyncContext
import qualified Lib.FSHook.Protocol as Protocol
import qualified Lib.Process as Process
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Directory as Dir
import qualified System.IO.Error as IOError

type AccessDoc = String

type JobId = ByteString

type InputHandler = AccessType -> AccessDoc -> FilePath -> IO ()
type OutputHandler = AccessDoc -> FilePath -> IO ()

data Handlers = Handlers
  { handleInput :: InputHandler
  , handleDelayedInput :: InputHandler
  , handleOutput :: OutputHandler
  }

data RunningJob = RunningJob
  { jobLabel :: String
  , jobActiveConnections :: IORef [MVar ()]
  , jobThreadId :: ThreadId
  , jobHandlers :: Handlers
  , jobRootFilter :: FilePath
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

data ProtocolError = ProtocolError String deriving (Show, Typeable)
instance E.Exception ProtocolError

serve :: FSHook -> Socket -> IO ()
serve fsHook conn = do
  helloLine <- SockBS.recv conn 1024
  case unprefixed (BS.pack "HELLO, I AM: ") helloLine of
    Nothing -> E.throwIO $ ProtocolError $ "Bad hello message from connection: " ++ show helloLine
    Just pidJobId -> do
      runningJobs <- readIORef (fsHookRunningJobs fsHook)
      case M.lookup jobId runningJobs of
        Nothing -> do
          let jobIds = M.keys runningJobs
          E.throwIO $ ProtocolError $ "Bad slave id: " ++ show jobId ++ " mismatches all: " ++ show jobIds
        Just job -> handleJobConnection fullTidStr conn job
      where
        fullTidStr = BS.unpack pidStr ++ ":" ++ BS.unpack tidStr
        [pidStr, tidStr, jobId] = BS.split ':' pidJobId

maxMsgSize :: Int
maxMsgSize = 8192

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
handleJobMsg _tidStr conn job msg =
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
      error $ unwords ["Hard links not supported:", show src, "->", show dest]
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
    handlers = jobHandlers job
    actDesc = Protocol.showFunc msg ++ " done by " ++ jobLabel job
    forwardExceptions =
      E.handle $ \e@E.SomeException {} -> E.throwTo (jobThreadId job) e
    reportInput accessType path
      | "/" `isPrefixOf` path =
        forwardExceptions $ handleInput handlers accessType actDesc path
      | otherwise = do
        forwardExceptions (handleDelayedInput handlers accessType actDesc path)
        sendGo conn
    reportOutput path =
      forwardExceptions $ handleOutput handlers actDesc path

handleJobConnection :: String -> Socket -> RunningJob -> IO ()
handleJobConnection tidStr conn job = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connFinishedMVar <- newEmptyMVar
  protect connFinishedMVar $ do
    atomicModifyIORef_ (jobActiveConnections job) (connFinishedMVar:)
    sendGo conn
    recvLoop_ maxMsgSize
      (handleJobMsg tidStr conn job . Protocol.parseMsg) conn
  where
    protect mvar act = act `E.finally` putMVar mvar ()

mkEnvVars :: FSHook -> FilePath -> JobId -> Process.Env
mkEnvVars fsHook rootFilter jobId =
    [ ("LD_PRELOAD", fsHookLdPreloadPath fsHook)
    , ("BUILDSOME_MASTER_UNIX_SOCKADDR", fsHookServerAddress fsHook)
    , ("BUILDSOME_JOB_ID", BS.unpack jobId)
    , ("BUILDSOME_ROOT_FILTER", rootFilter)
    ]

withRegistered :: Ord k => IORef (Map k a) -> k -> a -> IO r -> IO r
withRegistered registry jobId job =
  E.bracket_ registerRunningJob unregisterRunningJob
  where
    registerRunningJob = atomicModifyIORef_ registry $ M.insert jobId job
    unregisterRunningJob = atomicModifyIORef_ registry $ M.delete jobId

runCommand :: FSHook -> FilePath -> (Process.Env -> IO r) -> String -> Handlers -> IO r
runCommand fsHook rootFilter cmd label handlers = do
  activeConnections <- newIORef []
  jobIdNum <- nextJobId fsHook
  tid <- myThreadId

  let jobId = BS.pack ("cmd" ++ show jobIdNum)
      job = RunningJob
            { jobLabel = label
            , jobActiveConnections = activeConnections
            , jobThreadId = tid
            , jobRootFilter = rootFilter
            , jobHandlers = handlers
            }
  -- Don't leak connections still running our handlers once we leave!
  (`E.finally` awaitAllConnections activeConnections) $
    withRegistered (fsHookRunningJobs fsHook) jobId job $
    cmd (mkEnvVars fsHook rootFilter jobId)
  where
    awaitAllConnections activeConnections =
      -- Give all connections a chance to complete and perhaps fail
      -- this execution:
      mapM_ readMVar =<< readIORef activeConnections

accessDataFile :: FilePath -> FilePath -> (FilePath -> IO a) -> IO a
accessDataFile fallbackDir fileName accessor =
  (accessor =<< getDataFileName fileName)
  `E.catch` \e ->
    if IOError.isDoesNotExistError e
    then accessor $ fallbackDir </> fileName
    else E.throwIO e

getLdPreloadPath :: IO FilePath
getLdPreloadPath = do
  argv0 <- getArgv0
  accessDataFile (takeDirectory argv0) "fs_override.so" Dir.canonicalizePath
