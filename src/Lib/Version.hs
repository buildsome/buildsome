{-# LANGUAGE OverloadedStrings #-}
module Lib.Version
  ( get
  ) where

import Data.ByteString.Char8 (ByteString)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.Process as Process

get :: IO ByteString
get = do
  (exitCode, out, err) <- Process.getOutputs (Process.ShellCommand cmd) ["HOME", "USER"] []
  case exitCode of
    ExitSuccess -> return $ BS8.concat $ take 1 $ BS8.words out
    ExitFailure code -> do
      hPutStrLn stderr $ cmd ++ " failed: " ++ show code
      BS8.hPutStrLn stderr err
      return "unknown"
  where
    cmd = "git show-ref HEAD"
