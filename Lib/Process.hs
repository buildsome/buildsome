module Lib.Process (Process, makeProcess, waitProcess) where

import Control.Concurrent.Async
import Control.Monad
import System.Environment (getEnv, getProgName)
import System.Exit (ExitCode)
import System.Process
import qualified Data.ByteString.Char8 as BS
import qualified System.IO as IO

type Env = [(String, String)]

data Process = Process
  { _processCmd :: CmdSpec
  , _stdoutReader :: Async BS.ByteString
  , _stderrReader :: Async BS.ByteString
  , _exitCodeReader :: Async ExitCode
  }

makeProcess :: CmdSpec -> [String] -> Env -> IO Process
makeProcess cmd inheritedEnvs envs = do
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

