module Lib.PoolAlloc
  ( PoolAlloc, new
  , Priority(..)
  , startAlloc
  , Alloc, finish, changePriority
  , alloc, release
  ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (join)
import           Data.Function (on)
import           Data.IORef
import           Lib.Exception (onException)
import           Lib.IORef (atomicModifyIORef_)
import           Lib.PriorityQueue (PriorityQueue, Priority)
import qualified Lib.PriorityQueue as PriorityQueue
import qualified Lib.NonEmptyList as NonEmptyList
import           Lib.NonEmptyList (NonEmptyList(..))

import           Prelude.Compat

data PoolState a
    = PoolStateWithTokens (NonEmptyList a)
    | PoolStateWithoutTokens (PriorityQueue (MVar a))

newtype PoolAlloc a = PoolAlloc
    { _poolState :: IORef (PoolState a)
    }

new :: [a] -> IO (PoolAlloc a)
new []     = PoolAlloc <$> newIORef (PoolStateWithoutTokens PriorityQueue.empty)
new (x:xs) = PoolAlloc <$> newIORef (PoolStateWithTokens (NonEmptyList x xs))

data Alloc a = Alloc
    { finish :: IO a
    , changePriority :: Priority -> IO ()
    , identity :: IORef ()
    }
instance Eq (Alloc a) where
    (==) = (==) `on` identity

-- | start an allocation, and pure an action that blocks to finish
-- the allocation
-- NOTE: MUST run startAlloc with proper masking to avoid leaking the token!
--       MUST run the returned alloc action to avoid leaking!
startAlloc :: Priority -> PoolAlloc a -> IO (Alloc a)
startAlloc priority (PoolAlloc stateRef) =
    do
        candidate <- newEmptyMVar
        ident <- newIORef ()
        let
            trivialAlloc token =
                Alloc
                { finish = pure token
                , changePriority = const $ pure ()
                , identity = ident
                }
        atomicModifyIORef' stateRef $
            \case
            PoolStateWithoutTokens waiters ->
                ( PoolStateWithoutTokens (PriorityQueue.enqueue priority candidate waiters)
                , Alloc
                    { finish = takeMVar candidate `onException` stopAllocation candidate
                    , changePriority = (`changeAllocationPriority` candidate)
                    , identity = ident
                    }
                )
            PoolStateWithTokens (NonEmptyList token []) ->
                    (PoolStateWithoutTokens PriorityQueue.empty, trivialAlloc token)
            PoolStateWithTokens (NonEmptyList token (x:xs)) ->
                    (PoolStateWithTokens (NonEmptyList x xs), trivialAlloc token)
    where
        extract PoolStateWithTokens{} _candidate notThere _extracted = notThere
        extract (PoolStateWithoutTokens waiters) candidate notThere extracted =
            case PriorityQueue.extract priority (== candidate) waiters of
            (waiters', [_]) -> extracted waiters'
            (_, []) -> notThere
            (_, _) -> error "Expecting to find 0 or 1 candidate in waiter list"
        removeCandidate candidate ps =
            extract ps candidate
            -- Oops: Someone handed us over an allocation already, release it!
            (ps, takeMVar candidate >>= release (PoolAlloc stateRef))
            -- Got out in time, whew:
            $ \waiters' -> (PoolStateWithoutTokens waiters', pure ())
        changePriorityCandidate newPriority candidate ps =
            extract ps candidate
            -- Allocation already done, priority change irrelevant:
            ps
            -- Got out, re-insert with different priority:
            $ \waiters' -> PoolStateWithoutTokens $ PriorityQueue.enqueue newPriority candidate waiters'
        stopAllocation = join . atomicModifyIORef stateRef . removeCandidate
        changeAllocationPriority newPriority =
            atomicModifyIORef_ stateRef . changePriorityCandidate newPriority

-- NOTE: Must run alloc with proper masking!
alloc :: Priority -> PoolAlloc a -> IO a
alloc priority = (finish =<<) . startAlloc priority

-- | May release items that were never in the pool
release :: PoolAlloc a -> a -> IO ()
release (PoolAlloc stateRef) token =
  -- E.mask_ would be enough here, but I assume uninterruptibleMask_ is as cheap
  E.uninterruptibleMask_ $ join $ atomicModifyIORef stateRef f
  where
    f (PoolStateWithoutTokens waiters) =
      case PriorityQueue.dequeue waiters of
      Nothing -> (PoolStateWithTokens (NonEmptyList.singleton token), pure ())
      Just (newWaiters, (_priority, waiter)) ->
        (PoolStateWithoutTokens newWaiters, putMVar waiter token)
    f (PoolStateWithTokens tokens) = (PoolStateWithTokens (NonEmptyList.cons token tokens), pure ())
