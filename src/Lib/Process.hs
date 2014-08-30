module Lib.Process (getOutputs, Env, CmdSpec(..)) where

import Control.Concurrent.Async
import Control.Monad
import Data.Foldable (traverse_)
import Lib.Exception (bracket, finally)
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process
import qualified Data.ByteString.Char8 as BS

type Env = [(String, String)]

-- | Create a process, and kill it when leaving the given code section
withProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withProcess params =
  bracket (createProcess params) terminate
  where
    terminate (mStdin, mStdout, mStderr, processHandle) =
      interruptProcessGroupOf processHandle
        `finally` traverse_ hClose mStdin
        `finally` traverse_ hClose mStdout
        `finally` traverse_ hClose mStderr
        `finally` void (waitForProcess processHandle)

-- | Get the outputs of a process with a given environment spec
getOutputs :: CmdSpec -> [String] -> Env -> IO (ExitCode, BS.ByteString, BS.ByteString)
getOutputs cmd inheritedEnvs envs = do
  oldEnvs <- forM inheritedEnvs $ \name -> do
    val <- getEnv name
    return (name, val)
  withProcess
    -- A hacky way to get a default CreateProcess record for "process" package version compatability:
    (shell "")
    { cwd = Nothing
    , cmdspec = cmd
    , env = Just (oldEnvs ++ envs)
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = True -- MUST close fds so we don't leak server-side FDs as open/etc
    , create_group = True -- MUST be true so that interruptProcessGroupOf works
--    , delegate_ctlc = True
    } $ \(Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) -> do
    hClose stdinHandle
    withAsync (BS.hGetContents stdoutHandle) $ \stdoutReader ->
      withAsync (BS.hGetContents stderrHandle) $ \stderrReader -> do
        exitCode <- waitForProcess processHandle
        stdout <- wait stdoutReader
        stderr <- wait stderrReader
        return (exitCode, stdout, stderr)
