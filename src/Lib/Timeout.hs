{-# LANGUAGE RankNTypes #-}
module Lib.Timeout
  ( execute, warning
  , picos, nanos, micros, millis, seconds
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsyncWithUnmask)
import           Control.Exception (uninterruptibleMask)
import           Control.Monad (forever)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           Data.Time (DiffTime, picosecondsToDiffTime, secondsToDiffTime)
import           System.IO (stderr)

import           Prelude.Compat

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

execute :: DiffTime -> IO () -> IO a -> IO a
execute timeout timeoutAction body =
    -- Protect withAsyncWithUnmask with uninterruptibleMask
    uninterruptibleMask $ \restore ->
    withAsyncWithUnmask timeoutActionWrap $ \_async -> restore body
    where
        timeoutActionWrap :: (forall a. IO a -> IO a) -> IO ()
        timeoutActionWrap unmask =
            unmask $ do
                threadDelay $ floor $ 1000000.0 * timeout
                timeoutAction

warning :: DiffTime -> ByteString -> IO a -> IO a
warning timeout errMsg =
    execute timeout timeoutLoop
    where
        timeoutLoop =
            forever $
            do
                BS8.hPutStrLn stderr $ "TIMEOUT: " <> errMsg
                threadDelay $ floor $ 1000000.0 * timeout
