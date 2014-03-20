module Lib.Sigint (installSigintHandler) where

import Control.Concurrent (myThreadId)
import System.Posix.Signals (Handler(..), keyboardSignal, installHandler)
import qualified Control.Exception as E

installSigintHandler :: IO ()
installSigintHandler = do
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (E.throwTo tid E.UserInterrupt)) Nothing
  return ()
