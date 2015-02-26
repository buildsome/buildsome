module Lib.Sigint (withInstalledSigintHandler) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Lib.Exception (bracket)
import System.Posix.ByteString (Handler(..), keyboardSignal, installHandler)

withInstalledSigintHandler :: IO () -> IO a -> IO a
withInstalledSigintHandler action body =
    do
        mvar <- newEmptyMVar
        withAsync (takeMVar mvar >> action) $ \_ ->
            bracket (install mvar) uninstall $ const body
    where
        handleSigint handler =
            installHandler keyboardSignal handler Nothing
        install mvar = handleSigint $ Catch $ putMVar mvar ()
        uninstall oldHandler =
            do
                _ <- handleSigint oldHandler
                return ()
