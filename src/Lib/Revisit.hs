module Lib.Revisit
  ( M, avoid, run
  ) where

import           Control.Monad.Trans.State (StateT, evalStateT)
import           Data.Set (Set)
import qualified Control.Monad.Trans.State as State
import qualified Data.Set as Set

import           Prelude.Compat

-- Visited:
type M e m = StateT (Set e) m

avoid :: (Monad m, Ord e) => e -> M e m a -> M e m (Maybe a)
avoid rep act = do
  visited <- State.get
  if rep `Set.member` visited
    then return Nothing
    else do
      State.modify $ Set.insert rep
      Just <$> act

run :: Monad m => M e m a -> m a
run = flip evalStateT Set.empty
