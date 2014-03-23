module Lib.PoolAlloc
  ( PoolAlloc, new
  , startAlloc
  , alloc, release
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Exception (mask_)
import Control.Monad (join)
import Data.IORef
import Lib.Fifo (Fifo)
import qualified Lib.Fifo as Fifo

data PoolState a = PoolState
  { _freeItems :: [a]
  , _waiters :: Fifo (MVar a)
  }

newtype PoolAlloc a = PoolAlloc
  { _poolState :: IORef (PoolState a)
  }

new :: [a] -> IO (PoolAlloc a)
new xs = PoolAlloc <$> newIORef (PoolState xs Fifo.empty)

-- | start an allocation, and return an action that blocks to finish
-- the allocation
startAlloc :: PoolAlloc a -> IO (IO a)
startAlloc (PoolAlloc stateRef) = do
  candidate <- newEmptyMVar
  let f (PoolState [] waiters) = (PoolState [] (Fifo.enqueue candidate waiters), readMVar candidate)
      f (PoolState (x:xs) waiters)
        | Fifo.null waiters = (PoolState xs Fifo.empty, return x)
        | otherwise = error "Invariant broken: waiters when free elements exist (startAlloc)"
  atomicModifyIORef stateRef f

alloc :: PoolAlloc a -> IO a
alloc = join . startAlloc

-- | May release items that were never in the pool
release :: PoolAlloc a -> a -> IO ()
release (PoolAlloc stateRef) x =
  mask_ $ join $ atomicModifyIORef' stateRef f
  where
    f (PoolState [] waiters) =
      case Fifo.dequeue waiters of
      Nothing -> (PoolState [x] waiters, return ())
      Just (newWaiters, waiter) -> (PoolState [] newWaiters, putMVar waiter x)
    f (PoolState xs@(_:_) waiters)
      | Fifo.null waiters = (PoolState (x:xs) waiters, return ())
      | otherwise = error "Invariant broken: waiters exist when free elements exist (release)"
