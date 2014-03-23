module Lib.Fifo
  ( Fifo
  , empty, null
  , enqueue, dequeue
  ) where

import Prelude hiding (null)

data Fifo a = Fifo
  { _new :: [a] -- reverse order (fast prepend)
  , _old :: [a] -- forward order (fast pop-head)
  }

null :: Fifo a -> Bool
null (Fifo [] []) = True
null _ = False

empty :: Fifo a
empty = Fifo [] []

enqueue :: a -> Fifo a -> Fifo a
enqueue x (Fifo new old) = Fifo (x:new) old

dequeue :: Fifo a -> Maybe (Fifo a, a)
dequeue (Fifo new (o:os)) = Just (Fifo new os, o)
dequeue (Fifo new []) =
  case reverse new of
  [] -> Nothing
  (o:os) -> Just (Fifo [] os, o)
