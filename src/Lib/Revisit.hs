module Lib.Revisit
  ( M, avoid, run
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.State (State, evalState)
import Data.Set (Set)
import qualified Control.Monad.Trans.State as State
import qualified Data.Set as Set

-- Visited:
type M e = State (Set e)

avoid :: Ord e => e -> M e a -> M e (Maybe a)
avoid rep act = do
  visited <- State.get
  if rep `Set.member` visited
    then return Nothing
    else do
      State.modify $ Set.insert rep
      Just <$> act

run :: M e a -> a
run = flip evalState Set.empty
