{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.PriorityQueue
  ( Priority(..)
  , PriorityQueue
  , empty, null, enqueue, dequeue
  , extract
  ) where

import Prelude hiding (null)

import Control.Applicative ((<$>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Lib.Fifo (Fifo)
import qualified Data.Map as Map
import qualified Lib.Fifo as Fifo

newtype Priority = Priority Int
  deriving (Eq, Ord, Show, Num)

--- TODO: IntMap
newtype PriorityQueue a = PriorityQueue
  { _fifoPriority :: Map Priority (Fifo a) -- invariant: All fifos are non-empty
  }
empty :: PriorityQueue a
empty = PriorityQueue Map.empty

null :: PriorityQueue a -> Bool
null (PriorityQueue priorities) = Map.null priorities

enqueue :: Priority -> a -> PriorityQueue a -> PriorityQueue a
enqueue priority x (PriorityQueue priorities) =
  PriorityQueue $
  Map.alter (Just . Fifo.enqueue x . fromMaybe Fifo.empty) priority
  priorities

dequeue :: PriorityQueue a -> Maybe (PriorityQueue a, (Priority, a))
dequeue (PriorityQueue priorities) =
  f <$> Map.maxViewWithKey priorities
  where
    f ((priority, fifo), prioritiesWithoutMax) =
      case Fifo.dequeue fifo of
      Nothing -> error "Empty fifo inside map!"
      Just (fifo', x) ->
        (PriorityQueue priorities', (priority, x))
        where
          priorities'
            | Fifo.null fifo' = prioritiesWithoutMax
            | otherwise = Map.insert priority fifo' priorities

extract :: Priority -> (a -> Bool) -> PriorityQueue a -> (PriorityQueue a, [a])
extract priority p (PriorityQueue priorities) =
  case Map.lookup priority priorities of
  Nothing -> (PriorityQueue priorities, [])
  Just fifo ->
    ( PriorityQueue $ Map.insert priority fifo' priorities
    , xs )
    where
      (fifo', xs) = Fifo.extract p fifo
