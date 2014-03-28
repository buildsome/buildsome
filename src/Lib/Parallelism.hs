module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell, newCell
  , startAlloc
  , release, withReleased
  ) where

import Data.IORef
import Lib.PoolAlloc (PoolAlloc)
import qualified Control.Exception as E
import qualified Lib.PoolAlloc as PoolAlloc

type ParId = Int
type Cell = IORef ParId
type Parallelism = PoolAlloc ParId

new :: ParId -> IO Parallelism
new n = PoolAlloc.new [1..n]

startAlloc :: Parallelism -> IO (IO ParId)
startAlloc = PoolAlloc.startAlloc

release :: Parallelism -> Cell -> IO ()
release parallelism cell = PoolAlloc.release parallelism =<< readIORef cell

-- | Release the currently held item, run given action, then regain
-- new item instead
localReleasePool :: Parallelism -> Cell -> IO b -> IO b
localReleasePool parallelism cell =
  E.bracket_ (release parallelism cell >> markError) alloc
  where
    markError = writeIORef cell $ error "Attempt to read released resource"
    alloc = writeIORef cell =<< PoolAlloc.alloc parallelism

withReleased :: Cell -> Parallelism -> IO a -> IO a
withReleased cell parallelism = localReleasePool parallelism cell

newCell :: ParId -> IO Cell
newCell = newIORef
