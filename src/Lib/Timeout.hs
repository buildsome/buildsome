module Lib.Timeout
  ( warning
  , picos, nanos, micros, millis, seconds
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Data.Time (DiffTime, picosecondsToDiffTime, secondsToDiffTime)
import System.IO (hPutStrLn, stderr)

picos :: Integer -> DiffTime
picos = picosecondsToDiffTime

nanos :: Integer -> DiffTime
nanos = picos . (* 1000)

micros :: Integer -> DiffTime
micros = nanos . (* 1000)

millis :: Integer -> DiffTime
millis = micros . (* 1000)

seconds :: Integer -> DiffTime
seconds = secondsToDiffTime

warning :: DiffTime -> String -> IO a -> IO a
warning timeout errMsg action = do
  withAsync timeoutMsg $ \_ -> action
  where
    timeoutMsg = do
      threadDelay $ floor $ 1000000.0 * timeout
      hPutStrLn stderr $ "TIMEOUT: " ++ errMsg
