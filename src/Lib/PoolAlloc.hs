module Lib.PoolAlloc
  ( PoolAlloc, new
  , Priority(..)
  , startAlloc
  , Alloc, finish
  , alloc, release
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad (join)
import Data.IORef
import Lib.Exception (onException)
import Lib.PriorityQueue (PriorityQueue, Priority)
import qualified Control.Exception as E
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

newtype Alloc a = Alloc { finish :: IO a }

-- | start an allocation, and return an action that blocks to finish
-- the allocation
-- NOTE: MUST run startAlloc with proper masking to avoid leaking the token!
--       MUST run the returned alloc action to avoid leaking!
startAlloc :: Priority -> PoolAlloc a -> IO (Alloc a)
startAlloc priority (PoolAlloc stateRef) = do
  candidate <- newEmptyMVar
  let f (PoolState [] waiters) =
        ( PoolState [] (PriorityQueue.enqueue priority candidate waiters)
        , Alloc (takeMVar candidate `onException` stopAllocation candidate)
        )
      f (PoolState (x:xs) waiters)
        | PriorityQueue.null waiters = (PoolState xs waiters, Alloc (return x))
        | otherwise = error "Invariant broken: waiters when free elements exist (startAlloc)"
  atomicModifyIORef' stateRef f
  where
    removeCandidate candidate ps@(PoolState xs waiters) =
      case PriorityQueue.extract priority (== candidate) waiters of
        -- Got out in time, whew:
        (waiters', [_]) -> (PoolState xs waiters', return ())
        -- Oops: Someone handed us over an allocation already, release it!
        (_, []) -> (ps, takeMVar candidate >>= release (PoolAlloc stateRef))
        (_, _) -> error "Expecting to find 0 or 1 candidate in waiter list"
    stopAllocation candidate = join $ atomicModifyIORef stateRef (removeCandidate candidate)

-- NOTE: Must run alloc with proper masking!
alloc :: Priority -> PoolAlloc a -> IO a
alloc priority = join . fmap finish . startAlloc priority

-- | May release items that were never in the pool
release :: PoolAlloc a -> a -> IO ()
release (PoolAlloc stateRef) x =
  E.uninterruptibleMask_ $ join $ atomicModifyIORef stateRef f
  where
    f (PoolState [] waiters) =
      case PriorityQueue.dequeue waiters of
      Nothing -> (PoolState [x] waiters, return ())
      Just (newWaiters, (_priority, waiter)) ->
        (PoolState [] newWaiters, putMVar waiter x)
    f (PoolState xs@(_:_) waiters)
      | PriorityQueue.null waiters = (PoolState (x:xs) waiters, return ())
      | otherwise = error "Invariant broken: waiters exist when free elements exist (release)"
