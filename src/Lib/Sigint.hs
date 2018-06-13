module Lib.Sigint (withInstalledSigintHandler) where

import Control.Concurrent.Async (withAsyncWithUnmask)
import Control.Concurrent.MVar
import Control.Exception (uninterruptibleMask)
import Control.Monad (void)
import Lib.Exception (bracket)
import Lib.Once (once)
import System.Posix.ByteString (Handler(..), keyboardSignal, installHandler)

import Prelude.Compat

withInstalledSigintHandler :: IO () -> IO a -> IO a
withInstalledSigintHandler action body =
    do
        runOnce <- once
        let install mvar = handleSigint $ Catch $ void $ runOnce $ putMVar mvar ()
        mvar <- newEmptyMVar
        -- Protect withAsyncWithUnmask with uninterruptibleMask as it
        -- uses interruptible cleanups
        uninterruptibleMask $ \restore ->
            withAsyncWithUnmask (\unmask -> unmask (takeMVar mvar >> action)) $ \_ ->
            restore $ bracket (install mvar) uninstall $ const body
    where
        handleSigint handler =
            installHandler keyboardSignal handler Nothing
        uninstall oldHandler =
            do
                _ <- handleSigint oldHandler
                return ()
