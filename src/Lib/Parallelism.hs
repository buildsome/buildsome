{-# LANGUAGE DeriveDataTypeable #-}
module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , startAlloc
  , withReleased
  ) where

import Control.Exception.Async (catchSync, isAsynchronous)
import Control.Monad
import Data.IORef
import Data.Typeable (Typeable)
import Lib.PoolAlloc (PoolAlloc)
import qualified Control.Exception as E
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int


data CellState
  = CellKilled -- ^ async exception received during re-allocation
  | CellReleased
  | CellAlloced ParId

type Cell = IORef CellState
type Parallelism = PoolAlloc ParId

new :: ParId -> IO Parallelism
new n = PoolAlloc.new [1..n]

startAlloc :: Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc parallelism = do
  alloc <- PoolAlloc.startAlloc parallelism
  return $ E.bracket (newIORef . CellAlloced =<< alloc) (release parallelism)

data DoubleRelease = DoubleRelease deriving (Show, Typeable)
instance E.Exception DoubleRelease

release :: Parallelism -> Cell -> IO ()
release parallelism cell = E.mask_ $ do
  cellState <- atomicModifyIORef cell ((,) CellReleased)
  case cellState of
    CellAlloced parId -> PoolAlloc.release parallelism parId
    CellReleased -> E.throwIO DoubleRelease
    CellKilled -> return ()

onSyncException :: IO a -> IO () -> IO a
onSyncException body handler =
  body `catchSync` \e -> do
    handler
    E.throwIO e

onAsyncException :: IO a -> IO () -> IO a
onAsyncException body handler =
  body `E.catch` \e -> do
    when (isAsynchronous e) handler
    E.throwIO e

-- | Release the currently held item, run given action, then regain
-- new item instead
withReleased :: Cell -> Parallelism -> IO a -> IO a
withReleased cell parallelism body =
  E.mask $ \restore -> do
    release parallelism cell
    res <- protectAsync (restore body `onSyncException` realloc)
    protectAsync realloc
    return res
  where
    protectAsync = (`onAsyncException` writeIORef cell CellKilled)
    realloc = writeIORef cell . CellAlloced =<< PoolAlloc.alloc parallelism
