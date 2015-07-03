module Lib.PoolAlloc
  ( PoolAlloc, new
  , Priority(..)
  , startAlloc
  , Alloc, finish, changePriority
  , alloc, release
  ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (join)
import           Data.Function (on)
import           Data.IORef
import           Lib.Exception (onException)
import           Lib.IORef (atomicModifyIORef_)
import           Lib.PriorityQueue (PriorityQueue, Priority)
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

data Alloc a = Alloc
    { finish :: IO a
    , changePriority :: Priority -> IO ()
    , identity :: IORef ()
    }
instance Eq (Alloc a) where
    (==) = (==) `on` identity

-- | start an allocation, and return an action that blocks to finish
-- the allocation
-- NOTE: MUST run startAlloc with proper masking to avoid leaking the token!
--       MUST run the returned alloc action to avoid leaking!
startAlloc :: Priority -> PoolAlloc a -> IO (Alloc a)
startAlloc priority (PoolAlloc stateRef) = do
  candidate <- newEmptyMVar
  ident <- newIORef ()
  let f (PoolState [] waiters) =
        ( PoolState [] (PriorityQueue.enqueue priority candidate waiters)
        , Alloc
          { finish = takeMVar candidate `onException` stopAllocation candidate
          , changePriority = \newPriority -> changeAllocationPriority newPriority candidate
          , identity = ident
          }
        )
      f (PoolState (x:xs) waiters)
        | PriorityQueue.null waiters =
          ( PoolState xs waiters
          , Alloc
            { finish = return x
            , changePriority = const $ return ()
            , identity = ident
            }
          )
        | otherwise = error "Invariant broken: waiters when free elements exist (startAlloc)"
  atomicModifyIORef' stateRef f
  where
    extract waiters candidate notThere extracted =
      case PriorityQueue.extract priority (== candidate) waiters of
      (waiters', [_]) -> extracted waiters'
      (_, []) -> notThere
      (_, _) -> error "Expecting to find 0 or 1 candidate in waiter list"
    removeCandidate candidate ps@(PoolState xs waiters) =
      extract waiters candidate
      -- Oops: Someone handed us over an allocation already, release it!
      (ps, takeMVar candidate >>= release (PoolAlloc stateRef))
      -- Got out in time, whew:
      $ \waiters' -> (PoolState xs waiters', return ())
    changePriorityCandidate newPriority candidate ps@(PoolState xs waiters) =
      extract waiters candidate
      -- Allocation already done, priority change irrelevant:
      ps
      -- Got out, re-insert with different priority:
      $ \waiters' ->
        PoolState xs $ PriorityQueue.enqueue newPriority candidate waiters'
    stopAllocation = join . atomicModifyIORef stateRef . removeCandidate
    changeAllocationPriority newPriority =
        atomicModifyIORef_ stateRef . changePriorityCandidate newPriority

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
