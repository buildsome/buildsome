module Lib.Parallelism
  ( ParId
  , Parallelism, new
  , Cell
  , startAlloc
  , withReleased
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

startAlloc :: Parallelism -> IO ((Cell -> IO r) -> IO r)
startAlloc parallelism = do
  alloc <- PoolAlloc.startAlloc parallelism
  return $ E.bracket (newIORef =<< alloc) (release parallelism)

release :: Parallelism -> Cell -> IO ()
release parallelism cell = do
  parId <- readIORef cell
  E.mask_ $ do
    PoolAlloc.release parallelism parId
    writeIORef cell $ error "Attempt to read released resource"

-- | Release the currently held item, run given action, then regain
-- new item instead
localReleasePool :: Parallelism -> Cell -> IO b -> IO b
localReleasePool parallelism cell =
  E.bracket_
  (release parallelism cell)
  (writeIORef cell =<< PoolAlloc.alloc parallelism)

withReleased :: Cell -> Parallelism -> IO a -> IO a
withReleased cell parallelism = localReleasePool parallelism cell
