module Lib.Fifo
  ( Fifo
  , empty, null
  , enqueue, dequeue
  , extract
  ) where

import Prelude.Compat hiding (null)

import Data.List (partition)

data Fifo a = Fifo
  { _new :: [a] -- reverse order (fast prepend)
  , _old :: [a] -- forward order (fast pop-head)
  }

null :: Fifo a -> Bool
null (Fifo [] []) = True
null _ = False

empty :: Fifo a
empty = Fifo [] []

extract :: (a -> Bool) -> Fifo a -> (Fifo a, [a])
extract p (Fifo new old) = (Fifo newF oldF, newT ++ oldT)
  where
    (newT, newF) = partition p new
    (oldT, oldF) = partition p old

enqueue :: a -> Fifo a -> Fifo a
enqueue x (Fifo new old) = Fifo (x:new) old

dequeue :: Fifo a -> Maybe (Fifo a, a)
dequeue (Fifo new (o:os)) = Just (Fifo new os, o)
dequeue (Fifo new []) =
  case reverse new of
  [] -> Nothing
  (o:os) -> Just (Fifo [] os, o)
