module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , startAlloc
  , withReleased
  ) where

import Control.Concurrent.MVar
import Lib.PoolAlloc (PoolAlloc)
import qualified Control.Exception as E
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int
type Cell = MVar ParId
type Parallelism = PoolAlloc ParId

new :: ParId -> IO Parallelism
new n = PoolAlloc.new [1..n]

startAlloc :: Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc parallelism = do
  alloc <- PoolAlloc.startAlloc parallelism
  return $ E.bracket (newMVar =<< alloc) (release parallelism)

release :: Parallelism -> Cell -> IO ()
release parallelism cell = do
  -- Make sure parId is valid with forced evaluation
  parId <- takeMVar cell
  E.mask_ $ PoolAlloc.release parallelism parId

-- | Release the currently held item, run given action, then regain
-- new item instead
withReleased :: Cell -> Parallelism -> IO a -> IO a
withReleased cell parallelism =
  E.bracket_
  (release parallelism cell)
  (putMVar cell =<< PoolAlloc.alloc parallelism)
