{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module Lib.Exception
  ( finally
  , onException, onExceptionWith
  , catch, handle
  , bracket, bracket_, bracketOnError
  , logErrors
  , handleSync
  , putLn
  , swallowExceptions
  ) where

import qualified Control.Exception as E
#if MIN_VERSION_base(4,7,0)
import           Control.Exception (SomeAsyncException)
#else
import           Data.Typeable
#endif
import           Data.Maybe        (isJust)
import qualified System.IO         as IO

putLn :: IO.Handle -> String -> IO ()
putLn h = swallowExceptions . IO.hPutStrLn h

swallowExceptions :: IO () -> IO ()
swallowExceptions = E.uninterruptibleMask_ . handle (\E.SomeException {} -> return ())

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
  putLn IO.stderr (prefix ++ ": " ++ show e)

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket before after = E.bracket before (E.uninterruptibleMask_ . after)

bracket_ :: IO a -> IO () -> IO b -> IO b
bracket_ before after = E.bracket_ before (E.uninterruptibleMask_ after)

bracketOnError :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracketOnError before after = E.bracketOnError before (E.uninterruptibleMask_ . after)


---------------------------------------------------------------------------
-- The following was copied from asynchronous-exceptions, as that package has been deprecated (and
-- requires base < 4.8)
---------------------------------------------------------------------------

#if !MIN_VERSION_base(4,7,0)
-- | Exception class for asynchronous exceptions
data SomeAsyncException
  = forall e . E.Exception e => SomeAsyncException e
  deriving Typeable

instance E.Exception SomeAsyncException

instance Show SomeAsyncException where
  showsPrec p (SomeAsyncException e) = showsPrec p e
#endif

isAsynchronous :: E.SomeException -> Bool
isAsynchronous e =
  isJust (E.fromException e :: Maybe E.AsyncException) ||
  isJust (E.fromException e :: Maybe SomeAsyncException)

-- | Like 'catch', but catch any synchronous exceptions; let asynchronous ones pass through
catchSync :: IO a -> (E.SomeException -> IO a) -> IO a
catchSync a h = {-# SCC "catchSync" #-}
  E.catch a $ {-# SCC "catchSync.handle_exception" #-} \e ->
    if isAsynchronous e
      then E.throwIO e
      else h e

-- | Like 'handle', but catch any synchronous exceptions; let asynchronous ones pass through
handleSync :: (E.SomeException -> IO a) -> IO a -> IO a
handleSync = flip catchSync

---------------------------------------------------------------------------
