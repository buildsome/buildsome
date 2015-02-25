module Lib.Sigint (installSigintHandler) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.Posix.ByteString (Handler(..), keyboardSignal, installHandler)

-- TODO: withSigintHandler that cleans up both the SIGINT handler and
-- the forkIO?
installSigintHandler :: IO () -> IO ()
installSigintHandler handler = do
    mvar <- newEmptyMVar
    _ <- forkIO $ takeMVar mvar >> handler
    _ <- installHandler keyboardSignal (Catch (putMVar mvar ())) Nothing
    return ()
