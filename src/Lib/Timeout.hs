{-# LANGUAGE OverloadedStrings #-}
module Lib.Timeout
  ( warning
  , picos, nanos, micros, millis, seconds
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Time (DiffTime, picosecondsToDiffTime, secondsToDiffTime)
import System.IO (stderr)
import qualified Data.ByteString.Char8 as BS8

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

warning :: DiffTime -> ByteString -> IO a -> IO a
warning timeout errMsg action =
  withAsync timeoutMsg $ const action
  where
    timeoutMsg = forever $ do
      threadDelay $ floor $ 1000000.0 * timeout
      BS8.hPutStrLn stderr $ "TIMEOUT: " <> errMsg
