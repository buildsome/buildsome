module Lib.Process (shellCmdVerify, getOutputs, Env) where

import Control.Concurrent.Async
import Control.Monad
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process
import qualified Data.ByteString.Char8 as BS

type Env = [(String, String)]

-- | Get the outputs of a process with a given environment spec
getOutputs :: CmdSpec -> [String] -> Env -> IO (ExitCode, BS.ByteString, BS.ByteString)
getOutputs cmd inheritedEnvs envs = do
  oldEnvs <- forM inheritedEnvs $ \name -> do
    val <- getEnv name
    return (name, val)
  (Just stdinHandle, Just stdoutHandle, Just stderrHandle, processHandle) <- createProcess CreateProcess
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
  hClose stdinHandle
  withAsync (BS.hGetContents stdoutHandle) $ \stdoutReader ->
    withAsync (BS.hGetContents stderrHandle) $ \stderrReader -> do
      exitCode <- waitForProcess processHandle
      stdout <- wait stdoutReader
      stderr <- wait stderrReader
      return (exitCode, stdout, stderr)

shellCmdVerify :: [String] -> Env -> String -> IO ()
shellCmdVerify inheritEnvs newEnvs cmd = do
  (exitCode, stdout, stderr) <- getOutputs (ShellCommand cmd) inheritEnvs newEnvs
  showOutput "STDOUT" stdout
  showOutput "STDERR" stderr
  case exitCode of
    ExitFailure {} -> fail $ concat [show cmd, " failed!"]
    _ -> return ()
  where
    showOutput name bs
      | BS.null bs = return ()
      | otherwise = do
        putStrLn (name ++ ":")
        BS.putStr bs
