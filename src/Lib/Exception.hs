module Lib.Exception
  ( finally
  ) where

import qualified Control.Exception as E
import qualified System.IO as IO

infixl 1 `finally`
finally :: IO a -> IO () -> IO a
action `finally` cleanup =
  E.mask $ \restore -> do
    res <- restore action
      `catch` \e -> do
        logErrors ("overrides original error: " ++ show e) cleanup
        E.throwIO e
    logErrors "during successful finally cleanup" cleanup
    return res
  where
    catch :: IO a -> (E.SomeException -> IO a) -> IO a
    catch = E.catch
    logErrors suffix act =
      act `catch` \e -> do
      IO.hPutStrLn IO.stderr $ concat ["Finally clause error: ", show e, " ", suffix]
      E.throwIO e
