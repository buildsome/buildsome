{-# LANGUAGE DeriveDataTypeable #-}
module Lib.FSHook
  ( getLdPreloadPath
  , FSHook
  , with

  , OutputEffect(..), OutputBehavior(..)
  , Input(..)
  , DelayedOutput(..), UndelayedOutput
  , Protocol.OutFilePath(..), Protocol.OutEffect(..), Protocol.Severity(..)
  , FSAccessHandlers(..)

  , AccessType(..), AccessDoc

  , runCommand, timedRunCommand
  ) where

import           Control.Concurrent (ThreadId, myThreadId, killThread)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (forever, void, unless, (<=<))
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList)
import           Data.Monoid ((<>))
import           Data.Time (NominalDiffTime)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic(..))
import           Lib.Argv0 (getArgv0)
import qualified Lib.AsyncContext as AsyncContext
import           Lib.ByteString (unprefixed)
import           Lib.ColorText (ColorText)
import           Lib.Exception (finally, bracket_, onException, handleSync)
import           Lib.FSHook.AccessType (AccessType(..))
import           Lib.FSHook.OutputBehavior (OutputEffect(..), OutputBehavior(..))
import qualified Lib.FSHook.OutputBehavior as OutputBehavior
import           Lib.FSHook.Protocol (IsDelayed(..), Severity(..))
import qualified Lib.FSHook.Protocol as Protocol
import           Lib.FilePath (FilePath, (</>), takeDirectory, canonicalizePath)
import qualified Lib.FilePath as FilePath
import           Lib.Fresh (Fresh)
import qualified Lib.Fresh as Fresh
import           Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import           Lib.Printer (Printer)
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import           Lib.Sock (recvFrame, recvLoop_, withUnixStreamListener)
import           Lib.StdOutputs (StdOutputs(..))
import           Lib.TimeIt (timeIt)
import qualified Lib.Timeout as Timeout
import           Network.Socket (Socket)
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import           Paths_buildsome (getDataFileName)
import           System.Exit (ExitCode)
import           System.IO (hPutStrLn, stderr)
import qualified System.Posix.ByteString as Posix
import           System.Process (CmdSpec)

import           Prelude.Compat hiding (FilePath)

data AccessDoc
    = AccessDocEmpty -- TODO: AccessDoc Protocol.Func JobLabel
    deriving (Show, Generic)
instance Binary AccessDoc

type JobId = ByteString

data Input = Input
  { inputAccessType :: AccessType
  , inputPath :: FilePath
  } deriving (Eq, Ord, Show)

data DelayedOutput = DelayedOutput
-- TODO: Rename to delayedOutput...
  { outputBehavior :: OutputBehavior
  , outputPath :: FilePath
  } deriving (Eq, Ord, Show)

type UndelayedOutput = Protocol.OutFilePath

data FSAccessHandlers = FSAccessHandlers
  { delayedFSAccessHandler   :: AccessDoc -> [Input] -> [DelayedOutput] -> IO ()
  , undelayedFSAccessHandler :: AccessDoc -> [Input] -> [UndelayedOutput] -> IO ()
  , traceHandler             :: Severity -> ByteString -> IO ()
  }

type JobLabel = [FilePath]

data RunningJob = RunningJob
  { jobLabel :: JobLabel
  , jobLabelColorText :: ColorText
  , jobActiveConnections :: IORef (Map Int (ThreadId, IO ()))
  , jobFreshConnIds :: Fresh Int
  , jobThreadId :: ThreadId
  , jobFSAccessHandlers :: FSAccessHandlers
  , jobRootFilter :: FilePath
  }

data Job
  = KillingJob JobLabel ColorText
  | CompletedJob JobLabel ColorText
  | LiveJob RunningJob

data FSHook = FSHook
  { fsHookRunningJobs :: IORef (Map JobId Job)
  , fsHookFreshJobIds :: Fresh Int
  , fsHookLdPreloadPath :: FilePath
  , fsHookServerAddress :: FilePath
  , fsHookPrinter :: Printer
  }

newtype ProtocolError = ProtocolError String deriving (Typeable)
instance E.Exception ProtocolError
instance Show ProtocolError where
  show (ProtocolError msg) = "ProtocolError: " ++ msg

data Need = HOOK | HINT
fromNeedStr :: ByteString -> Need
fromNeedStr "HOOK" = HOOK
fromNeedStr "HINT" = HINT
fromNeedStr x = error $ "Invalid hello message need str: " ++ show x

serve :: Printer -> FSHook -> Socket -> IO ()
serve printer fsHook conn = do
  mHelloLine <- recvFrame conn
  case mHelloLine of
    Nothing -> E.throwIO $ ProtocolError "Unexpected EOF"
    Just helloLine ->
      case unprefixed Protocol.helloPrefix helloLine of
        Nothing ->
          E.throwIO $ ProtocolError $ concat
          [ "Bad hello message from connection: ", show helloLine, " expected: "
          , show Protocol.helloPrefix, " (check your fs_override.so installation)" ]
        Just pidJobId -> do
          (pidStr, tidStr, jobId, needStr) <-
            case BS8.split ':' pidJobId of
            [a,b,c,d] -> return (a,b,c,d)
            _ -> E.throwIO $ ProtocolError $ "Bad pid/jobid: " ++ BS8.unpack pidJobId
          let fullTidStr = concat [BS8.unpack pidStr, ":", BS8.unpack tidStr]

          runningJobs <- readIORef (fsHookRunningJobs fsHook)
          case M.lookup jobId runningJobs of
            Nothing -> do
              let jobIds = M.keys runningJobs
              E.throwIO $ ProtocolError $ concat ["Bad slave id: ", show jobId, " mismatches all: ", show jobIds]
            Just (KillingJob _label _labelColorText) ->
              -- New connection created in the process of killing connections, ignore it
              return ()
            Just (LiveJob job) ->
              handleJobConnection fullTidStr conn job $ fromNeedStr needStr
            Just (CompletedJob _label labelStr) ->
              E.throwIO $ ProtocolError $ concat
              -- Main/parent process completed, and leaked some subprocess
              -- which connected again!
              [ "Job: ", BS8.unpack jobId, "(", BS8.unpack (Printer.render printer labelStr)
              , ") received new connections after formal completion!"]

-- Except thread killed
printRethrowExceptions :: String -> IO a -> IO a
printRethrowExceptions msg =
  E.handle $ \e -> do
    case E.fromException e of
      Just E.ThreadKilled -> return ()
      _ -> E.uninterruptibleMask_ $ hPutStrLn stderr $ msg ++ show (e :: E.SomeException)
    E.throwIO e

with :: Printer -> FilePath -> (FSHook -> IO a) -> IO a
with printer ldPreloadPath body = do
  pid <- Posix.getProcessID
  freshJobIds <- Fresh.new 0
  let serverFilename = "/tmp/fshook-" <> BS8.pack (show pid)
  withUnixStreamListener serverFilename $ \listener -> do
    runningJobsRef <- newIORef M.empty
    let
      fsHook = FSHook
        { fsHookRunningJobs = runningJobsRef
        , fsHookFreshJobIds = freshJobIds
        , fsHookLdPreloadPath = ldPreloadPath
        , fsHookServerAddress = serverFilename
        , fsHookPrinter = printer
        }
    AsyncContext.new $ \ctx -> do
      _ <-
        AsyncContext.spawn ctx $ printRethrowExceptions "BUG: Listener loop threw exception: " $ forever $
        do
          (conn, _srcAddr) <- Sock.accept listener
          AsyncContext.spawn ctx $
            -- Job connection may fail when the process is killed
            -- during a send-message, which may cause a protocol error
            printRethrowExceptions "Job connection failed: " $
            serve printer fsHook conn
            `finally` Sock.close conn
      body fsHook

{-# INLINE sendGo #-}
sendGo :: Socket -> IO ()
sendGo conn = void $ SockBS.send conn (BS8.pack "GO")

{-# INLINE handleJobMsg #-}
handleJobMsg :: String -> Socket -> RunningJob -> Protocol.Msg -> IO ()
handleJobMsg _tidStr conn job (Protocol.Msg isDelayed func) =
  case func of
    -- TODO: If any of these outputs are NOT also mode-only inputs on
    -- their file paths, don't use handleOutputs so that we don't
    -- report them as inputs

    -- outputs
    -- TODO: Handle truncation flag
    Protocol.OpenW outPath _openWMode _create Protocol.OpenNoTruncate
                                -> handleOutputs [(OutputBehavior.fileChanger, outPath)]
    Protocol.OpenW outPath _openWMode _create Protocol.OpenTruncate
                                -> handleOutputs [(OutputBehavior.fileChanger, outPath)]
    Protocol.Creat outPath _    -> handleOutputs [(OutputBehavior.fileChanger, outPath)]
    Protocol.Rename a b         -> handleOutputs [ (OutputBehavior.existingFileChanger, a)
                                                 , (OutputBehavior.fileChanger, b) ]
    Protocol.Unlink outPath _   -> handleOutputs [(OutputBehavior.existingFileChanger, outPath)]
    Protocol.Truncate outPath _ -> handleOutputs [(OutputBehavior.existingFileChanger, outPath)]
    Protocol.Chmod outPath _    -> handleOutputs [(OutputBehavior.existingFileChanger, outPath)]
    Protocol.Chown outPath _ _  -> handleOutputs [(OutputBehavior.existingFileChanger, outPath)]
    Protocol.MkNod outPath _ _  -> handleOutputs [(OutputBehavior.nonExistingFileChanger, outPath)] -- TODO: Special mkNod handling?
    Protocol.MkDir outPath _    -> handleOutputs [(OutputBehavior.nonExistingFileChanger, outPath)]
    Protocol.RmDir outPath      -> handleOutputs [(OutputBehavior.existingFileChanger, outPath)]

    -- I/O
    Protocol.SymLink target linkPath ->
      -- TODO: We don't actually read the input here, but we don't
      -- handle symlinks correctly yet, so better be false-positive
      -- than false-negative
      handle
        [ Input AccessTypeFull target ]
        [ (OutputBehavior.nonExistingFileChanger, linkPath) ]
    Protocol.Link src dest -> error $ unwords ["Hard links not supported:", show src, "->", show dest]

    -- inputs
    Protocol.OpenR path            -> handleInput AccessTypeFull path
    Protocol.Access path _mode     -> handleInput AccessTypeModeOnly path
    Protocol.Stat path             -> handleInput AccessTypeStat path
    Protocol.LStat path            -> handleInput AccessTypeStat path
    Protocol.OpenDir path          -> handleInput AccessTypeFull path
    Protocol.ReadLink path         -> handleInput AccessTypeFull path
    Protocol.Exec path             -> handleInput AccessTypeFull path
    Protocol.ExecP mPath attempted ->
      handleInputs $
      map (Input AccessTypeFull) (maybeToList mPath) ++
      map (Input AccessTypeModeOnly) attempted
    Protocol.RealPath path         -> handleInput AccessTypeModeOnly path
    Protocol.Trace severity msg    -> traceHandler handlers severity msg
  where
    handlers = jobFSAccessHandlers job
    handleDelayed   inputs outputs = wrap $ delayedFSAccessHandler handlers actDesc inputs outputs
    handleUndelayed inputs outputs = wrap $ undelayedFSAccessHandler handlers actDesc inputs outputs
    wrap = wrapHandler job conn isDelayed
    actDesc = AccessDocEmpty -- TODO: AccessDoc func $ jobLabel job
    handleInput accessType path = handleInputs [Input accessType path]
    handleInputs inputs =
      case isDelayed of
      NotDelayed -> handleUndelayed inputs []
      Delayed    -> handleDelayed   inputs []
    inputOfOutputPair (_behavior, Protocol.OutFilePath path _effect) =
      Input AccessTypeModeOnly path
    mkDelayedOutput ( behavior, Protocol.OutFilePath path _effect) =
      DelayedOutput behavior path
    handleOutputs = handle []
    handle inputs outputPairs =
      case isDelayed of
      NotDelayed -> handleUndelayed allInputs $ map snd outputPairs
      Delayed -> handleDelayed allInputs $ map mkDelayedOutput outputPairs
      where
        allInputs = inputs ++ map inputOfOutputPair outputPairs

wrapHandler :: RunningJob -> Socket -> IsDelayed -> IO () -> IO ()
wrapHandler job conn isDelayed handler =
  forwardExceptions $ do
    handler
    -- Intentionally avoid sendGo if jobFSAccessHandler failed. It
    -- means we disallow the effect.
    case isDelayed of
      Delayed -> sendGo conn
      NotDelayed -> return ()
  where
    forwardExceptions =
      handleSync $ \e@E.SomeException {} ->
      E.throwTo (jobThreadId job) e

withRegistered :: Ord k => IORef (Map k a) -> k -> a -> IO r -> IO r
withRegistered registry key val =
  bracket_ register unregister
  where
    register = atomicModifyIORef_ registry $ M.insert key val
    unregister = atomicModifyIORef_ registry $ M.delete key

handleJobConnection :: String -> Socket -> RunningJob -> Need -> IO ()
handleJobConnection tidStr conn job _need = do
  -- This lets us know for sure that by the time the slave dies,
  -- we've seen its connection
  connId <- Fresh.next $ jobFreshConnIds job
  tid <- myThreadId

  connFinishedMVar <- newEmptyMVar
  (`finally` putMVar connFinishedMVar ()) $
    withRegistered (jobActiveConnections job) connId (tid, readMVar connFinishedMVar) $ do
      sendGo conn
      recvLoop_ (handleJobMsg tidStr conn job <=< Protocol.parseMsg) conn

mkEnvVars :: FSHook -> FilePath -> JobId -> Process.Env
mkEnvVars fsHook rootFilter jobId =
  (map . fmap) BS8.unpack
  [ ("LD_PRELOAD", fsHookLdPreloadPath fsHook)
  , ("DYLD_FORCE_FLAT_NAMESPACE", "1")
  , ("DYLD_INSERT_LIBRARIES", fsHookLdPreloadPath fsHook)
  , ("BUILDSOME_MASTER_UNIX_SOCKADDR", fsHookServerAddress fsHook)
  , ("BUILDSOME_JOB_ID", jobId)
  , ("BUILDSOME_ROOT_FILTER", rootFilter)
    -- Tell python to not generate .pyc files. If the users want them,
    -- they can generate them with an explicit rule, such as:
    --
    --   $./%.pyc: $./%.py
    --           pycompile "$<"
    --
  , ("PYTHONDONTWRITEBYTECODE", "1")
  ]

timedRunCommand ::
  FSHook -> FilePath -> [String] -> CmdSpec -> JobLabel -> ColorText ->
  FSAccessHandlers -> IO (NominalDiffTime, (ExitCode, StdOutputs ByteString))
timedRunCommand fsHook rootFilter inheritEnvs cmdSpec label labelColorText fsAccessHandlers = do
  pauseTimeRef <- newIORef 0
  let
    addPauseTime delta = atomicModifyIORef'_ pauseTimeRef (+delta)
    measurePauseTime act = do
      (time, res) <- timeIt act
      addPauseTime time
      return res
    wrappedFsAccessHandler isDelayed handler accessDoc inputs outputs = do
      let act = handler accessDoc inputs outputs
      case isDelayed of
        Delayed -> measurePauseTime act
        NotDelayed -> act
  let
    FSAccessHandlers oldDelayed oldUndelayed oldTraceHandler = fsAccessHandlers
    wrappedFsAccessHandlers =
      FSAccessHandlers
        (wrappedFsAccessHandler Delayed oldDelayed)
        (wrappedFsAccessHandler NotDelayed oldUndelayed)
        oldTraceHandler
  (time, res) <-
    timeIt $
    runCommand fsHook rootFilter inheritEnvs cmdSpec
    label labelColorText wrappedFsAccessHandlers
  subtractedTime <- (time-) <$> readIORef pauseTimeRef
  return (subtractedTime, res)

withRunningJob :: FSHook -> JobId -> RunningJob -> IO r -> IO r
withRunningJob fsHook jobId job body = do
  setJob (LiveJob job)
  (body <* setJob (CompletedJob (jobLabel job) (jobLabelColorText job)))
    `onException` setJob (KillingJob (jobLabel job) (jobLabelColorText job))
  where
    registry = fsHookRunningJobs fsHook
    setJob = atomicModifyIORef_ registry . M.insert jobId

runCommand ::
  FSHook -> FilePath -> [String] -> CmdSpec -> JobLabel -> ColorText ->
  FSAccessHandlers -> IO (ExitCode, StdOutputs ByteString)
runCommand fsHook rootFilter inheritEnvs cmdSpec label labelColorText fsAccessHandlers =
  do
    activeConnections <- newIORef M.empty
    freshConnIds <- Fresh.new 0
    jobIdNum <- Fresh.next $ fsHookFreshJobIds fsHook
    tid <- myThreadId

    let jobId = BS8.pack ("cmd" ++ show jobIdNum)
        job = RunningJob
              { jobLabel = label
              , jobLabelColorText = labelColorText
              , jobActiveConnections = activeConnections
              , jobFreshConnIds = freshConnIds
              , jobThreadId = tid
              , jobRootFilter = rootFilter
              , jobFSAccessHandlers = fsAccessHandlers
              }
    -- Don't leak connections still running our handlers once we leave!
    let onActiveConnections f = mapM_ f . M.elems =<< readIORef activeConnections
    (`onException` onActiveConnections killConnection) $
      do
        (exitCode, stdOutputs) <-
          withRunningJob fsHook jobId job $ runCmd $ mkEnvVars fsHook rootFilter jobId
        let timeoutMsg =
              Printer.render (fsHookPrinter fsHook)
              (labelColorText <> ": Process completed, but still has likely-leaked " <>
               "children connected to FS hooks")
        Timeout.warning (Timeout.seconds 5) timeoutMsg $
          onActiveConnections awaitConnection
        return (exitCode, stdOutputs)
  where
    runCmd = Process.getOutputs cmdSpec inheritEnvs
    killConnection (tid, awaitConn) = killThread tid >> awaitConn
    awaitConnection (_tid, awaitConn) = awaitConn

data CannotFindOverrideSharedObject = CannotFindOverrideSharedObject deriving (Show, Typeable)
instance E.Exception CannotFindOverrideSharedObject

assertLdPreloadPathExists :: FilePath -> IO ()
assertLdPreloadPathExists path = do
  e <- FilePath.exists path
  unless e $ E.throwIO CannotFindOverrideSharedObject

getLdPreloadPath :: Maybe FilePath -> IO FilePath
getLdPreloadPath (Just path) = do
  ldPreloadPath <- canonicalizePath path
  assertLdPreloadPathExists ldPreloadPath
  return ldPreloadPath
getLdPreloadPath Nothing = do
  installedFilePath <- BS8.pack <$> (getDataFileName . BS8.unpack) fileName
  installedExists <- FilePath.exists installedFilePath
  if installedExists
    then return installedFilePath
    else do
      argv0 <- getArgv0
      let nearExecPath = takeDirectory argv0 </> fileName
      assertLdPreloadPathExists nearExecPath
      return nearExecPath
  where
    fileName = "cbits/fs_override.so"
