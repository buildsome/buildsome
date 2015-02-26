module Lib.Sigint (withInstalledSigintHandler) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.Monad (void)
import Lib.Exception (bracket)
import Lib.Once (once)
import System.Posix.ByteString (Handler(..), keyboardSignal, installHandler)

withInstalledSigintHandler :: IO () -> IO a -> IO a
withInstalledSigintHandler action body =
    do
        runOnce <- once
        let install mvar = handleSigint $ Catch $ void $ runOnce $ putMVar mvar ()
        mvar <- newEmptyMVar
        withAsync (takeMVar mvar >> action) $ \_ ->
            bracket (install mvar) uninstall $ const body
    where
        handleSigint handler =
            installHandler keyboardSignal handler Nothing
        uninstall oldHandler =
            do
                _ <- handleSigint oldHandler
                return ()
