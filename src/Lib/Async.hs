module Lib.Async
  ( wrapAsync
  ) where

import Control.Concurrent.Async

wrapAsync :: Async a -> (a -> IO b) -> IO (Async b)
wrapAsync a f = async (f =<< wait a)
