module Lib.Exception
  ( finally
  , onException, onExceptionWith
  , catch, handle
  , bracket, bracket_
  , logErrors
  ) where

import qualified Control.Exception as E
import qualified System.IO as IO

infixl 1 `finally`
finally :: IO a -> IO () -> IO a
action `finally` cleanup =
  E.mask $ \restore -> do
    res <- restore action
      `catch` \e@E.SomeException {} -> do
        cleanup `logErrors` ("overrides original error (" ++ show e ++ ")")
        E.throwIO e
    E.uninterruptibleMask_ cleanup `logErrors` "during successful finally cleanup"
    return res

infixl 1 `catch`
catch :: E.Exception e => IO a -> (e -> IO a) -> IO a
act `catch` handler = act `E.catch` (E.uninterruptibleMask_ . handler)

handle :: E.Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch

infixl 1 `onException`
onException :: IO a -> IO b -> IO a
onException act handler = E.onException act (E.uninterruptibleMask_ handler)

infixl 1 `onExceptionWith`
{-# INLINE onExceptionWith #-}
onExceptionWith :: IO a -> (E.SomeException -> IO ()) -> IO a
onExceptionWith act f =
  act `E.catch` \e -> E.uninterruptibleMask_ (f e) >> E.throwIO e

infixl 1 `logErrors`
logErrors :: IO a -> String -> IO a
logErrors act prefix = onExceptionWith act $ \e ->
  IO.hPutStrLn IO.stderr $ prefix ++ ": " ++ show e

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket before after = E.bracket before (E.uninterruptibleMask_ . after)

bracket_ :: IO a -> IO () -> IO b -> IO b
bracket_ before after = E.bracket_ before (E.uninterruptibleMask_ after)
