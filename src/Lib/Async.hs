{-# LANGUAGE RankNTypes #-}
-- | Wrap the Async API with one that does not leak ThreadIds when the
-- async thread completes
{-# LANGUAGE LambdaCase #-}
module Lib.Async
     ( Async
     , async
     , asyncWithUnmask
     , wait
     , waitCatch
     , cancel
     ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import           Data.IORef
import           Lib.IORef

data Result a
    = Error !E.SomeException
    | Success !a

data State a
    = Running !(Async.Async a)
    | Done !(Result a)
    | Starting

newtype Async a = Async (IORef (State a))

mkResult :: Either E.SomeException a -> Result a
mkResult = either Error Success

fromResult :: Result a -> Either E.SomeException a
fromResult (Error err) = Left err
fromResult (Success x) = Right x

-- Use this weird form because "writeIORef" might not give as strong
-- guarantees as atomicModifyIORef?
writeIORef' :: IORef a -> a -> IO ()
writeIORef' ioref = atomicModifyIORef'_ ioref . const

untry :: Result a -> IO a
untry (Error err) = E.throwIO err
untry (Success x) = return x

asyncBase :: ((IO a -> IO a) -> IO (Async.Async a)) -> IO (Async a)
asyncBase createAsync =
    do
        ref <- newIORef Starting
        a <-
            createAsync $ \action ->
            E.uninterruptibleMask $ \restore ->
            do
                res <- mkResult <$> E.try (restore action)
                writeIORef' ref $ Done res
                untry res
        atomicModifyIORef'_ ref $ \old ->
            case old of
            Starting -> Running a
            Done _ -> old
            Running _ -> error "Set to running twice?!"
        return (Async ref)

async :: IO a -> IO (Async a)
async action = asyncBase $ \wrap -> Async.async (wrap action)

asyncWithUnmask :: ((forall b. IO b -> IO b) -> IO a) -> IO (Async a)
asyncWithUnmask action = asyncBase $ \wrap -> Async.asyncWithUnmask (\unmask -> wrap (action unmask))

op ::
    (Async.Async a -> IO b) ->
    (Result a -> IO b) ->
    Async a -> IO b
op running done (Async ioref) =
    readIORef ioref >>= \case
    Running a -> running a
    Done res -> done res
    Starting -> error "An Async value with a Starting value was exposed?!"

wait :: Async a -> IO a
wait = op Async.wait untry

waitCatch :: Async a -> IO (Either E.SomeException a)
waitCatch = op Async.waitCatch (return . fromResult)

cancel :: Async a -> IO ()
cancel = op Async.cancel $ const (return ())
