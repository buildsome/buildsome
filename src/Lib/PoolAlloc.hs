module Lib.PoolAlloc
  ( PoolAlloc, new
  , Priority(..)
  , startAlloc
  , alloc, release
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Exception (mask_)
import Control.Monad (join)
import Data.IORef
import Lib.PriorityQueue (PriorityQueue, Priority)
import qualified Lib.PriorityQueue as PriorityQueue

data PoolState a = PoolState
  { _freeItems :: [a]
  , _waiters :: PriorityQueue (MVar a)
  }

newtype PoolAlloc a = PoolAlloc
  { _poolState :: IORef (PoolState a)
  }

new :: [a] -> IO (PoolAlloc a)
new xs = PoolAlloc <$> newIORef (PoolState xs PriorityQueue.empty)

-- | start an allocation, and return an action that blocks to finish
-- the allocation
startAlloc :: Priority -> PoolAlloc a -> IO (IO a)
startAlloc priority (PoolAlloc stateRef) = do
  candidate <- newEmptyMVar
  let f (PoolState [] waiters) =
        ( PoolState [] (PriorityQueue.enqueue priority candidate waiters)
        , readMVar candidate )
      f (PoolState (x:xs) waiters)
        | PriorityQueue.null waiters = (PoolState xs waiters, return x)
        | otherwise = error "Invariant broken: waiters when free elements exist (startAlloc)"
  atomicModifyIORef stateRef f

alloc :: Priority -> PoolAlloc a -> IO a
alloc priority = join . startAlloc priority

-- | May release items that were never in the pool
release :: PoolAlloc a -> a -> IO ()
release (PoolAlloc stateRef) x =
  mask_ $ join $ atomicModifyIORef' stateRef f
  where
    f (PoolState [] waiters) =
      case PriorityQueue.dequeue waiters of
      Nothing -> (PoolState [x] waiters, return ())
      Just (newWaiters, waiter) -> (PoolState [] newWaiters, putMVar waiter x)
    f (PoolState xs@(_:_) waiters)
      | PriorityQueue.null waiters = (PoolState (x:xs) waiters, return ())
      | otherwise = error "Invariant broken: waiters exist when free elements exist (release)"
