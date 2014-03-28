module Lib.Async
  ( wrapAsync
  , verifyCancelled
  ) where

import Control.Concurrent.Async
import qualified Control.Exception as E

wrapAsync :: Async a -> (a -> IO b) -> IO (Async b)
wrapAsync a f = async (f =<< wait a)

verifyCancelled :: Async a -> IO (Either E.SomeException a)
verifyCancelled a = do
  cancel a
  waitCatch a
