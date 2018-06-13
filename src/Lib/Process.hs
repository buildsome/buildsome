{-# LANGUAGE TupleSections #-}
-- | Low level wrapping over System.Process
module Lib.Process (getOutputs, Env, CmdSpec(..)) where

import           Control.Concurrent.Async
import           Control.Exception (uninterruptibleMask)
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (traverse_)
import           Data.Maybe (catMaybes)
import           Lib.Exception (bracket, finally)
import           Lib.StdOutputs (StdOutputs(..))
import qualified Lib.Timeout as Timeout
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.IO (Handle, hClose)
import           System.Process

import           Prelude.Compat

type Env = [(String, String)]

timeoutTerminateProcess :: ProcessHandle -> IO a -> IO a
timeoutTerminateProcess processHandle =
  Timeout.execute (Timeout.seconds 2) $ terminateProcess processHandle

-- | Create a process, and terminate it when leaving the given code section
withProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withProcess params =
  bracket (createProcess params) terminate
  where
    terminate (mStdin, mStdout, mStderr, processHandle) =
      timeoutTerminateProcess processHandle
      ( interruptProcessGroupOf processHandle
        `finally` traverse_ hClose mStdin
        `finally` traverse_ hClose mStdout
        `finally` traverse_ hClose mStderr
        `finally` void (waitForProcess processHandle)
      )

-- | Get the outputs of a process with a given environment spec
getOutputs :: CmdSpec -> [String] -> Env -> IO (ExitCode, StdOutputs BS.ByteString)
getOutputs cmd inheritedEnvs envs = do
  oldEnvs <- catMaybes <$> forM inheritedEnvs (\name -> fmap (name,) <$> lookupEnv name)
  withProcess
    CreateProcess
    { cwd = Nothing
    , cmdspec = cmd
    , env = Just (oldEnvs ++ envs)
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = True -- MUST close fds so we don't leak server-side FDs as open/etc
    , create_group = True -- MUST be true so that interruptProcessGroupOf works
    , delegate_ctlc = False -- MUST be false to avoid disabling buildsome's SIGINT/SIGQUIT handlers
    , detach_console = False
    , create_new_console = False
    , new_session = False
    , child_group = Nothing
    , child_user = Nothing
    , use_process_jobs = False
    } $ \case
      (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) -> do
        hClose stdinHandle
        -- Read both stdout and stderr concurrently to prevent deadlock - e.g. if we read them
        -- sequentially (stdout then stderr) and the child writes to both interleaved, it can block
        -- indefinitely on stderr due to full buffers since we are not reading it yet.

        -- NOTE: withAsync has unsafe interruptible exception handler,
        -- protect it with uninterruptibleMask_
        uninterruptibleMask $ \restore ->
          withAsyncWithUnmask (\unmask -> unmask (BS.hGetContents stdoutHandle)) $ \stdoutReader ->
          withAsyncWithUnmask (\unmask -> unmask (BS.hGetContents stderrHandle)) $ \stderrReader ->
          restore $ do
            exitCode <- waitForProcess processHandle
            stdout <- wait stdoutReader
            stderr <- wait stderrReader
            return (exitCode, StdOutputs { stdOut = stdout, stdErr = stderr })
      _ -> error "withProcess didn't supply handles?!"
