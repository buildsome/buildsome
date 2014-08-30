{-# LANGUAGE OverloadedStrings #-}
module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , Priority(..)
  , startAlloc
  , withReleased
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Lib.Exception (bracket, bracket_, finally)
import Lib.IORef (atomicModifyIORef_)
import Lib.PoolAlloc (PoolAlloc, Priority(..))
import qualified Control.Exception as E
import qualified Lib.PoolAlloc as PoolAlloc

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and regained once. The regain will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

type ParId = Int

data CellState
  = CellReleased Int (MVar ()) -- ^ Release overdraft and mvar to publish alloc result when allocation succeeds
  | CellAlloced ParId
  | CellAllocating (MVar ())

type Cell = IORef CellState
type Parallelism = PoolAlloc ParId

new :: ParId -> IO Parallelism
new n = PoolAlloc.new [1..n]

-- MUST be called under proper masking to avoid losing result bracket
-- which MUST be invoked to avoid a leak!
startAlloc :: Priority -> Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc priority parallelism = do
  alloc <- PoolAlloc.startAlloc priority parallelism
  return $ bracket (newIORef . CellAlloced =<< alloc) (release parallelism)

release :: Parallelism -> Cell -> IO ()
release parallelism cell = go
  where
    go = do
      mvar <- newEmptyMVar
      E.mask_ $ do
        join $ atomicModifyIORef cell $ \cellState ->
          case cellState of
          CellReleased n oldMVar -> (CellReleased (n + 1) oldMVar, return ())
          CellAlloced parId      -> (CellReleased 0          mvar, PoolAlloc.release parallelism parId)
          CellAllocating oldMVar -> (cellState, readMVar oldMVar >> go)

-- | Release the currently held item, run given action, then regain
-- new item instead
withReleased :: Priority -> Cell -> Parallelism -> IO a -> IO a
withReleased priority cell parallelism =
  bracket_ (release parallelism cell) realloc
  where
    setAlloced parId = atomicModifyIORef_ cell $ \cellState ->
      case cellState of
      CellAllocating _ -> CellAlloced parId
      _ -> error "Somebody touched the cell when it was in CellAllocating?!"
    actualAlloc mvar =
      (PoolAlloc.alloc priority parallelism >>= setAlloced)
      `finally` putMVar mvar ()
    realloc =
      join $ atomicModifyIORef cell $ \cellState ->
      case cellState of
      CellReleased 0 mvar -> (CellAllocating mvar, actualAlloc mvar)
      CellReleased n mvar -> (CellReleased (n-1) mvar, return ())
      CellAllocating mvar -> (CellAllocating mvar, readMVar mvar >> realloc)
      CellAlloced _       -> error "More allocs than releases?!"
