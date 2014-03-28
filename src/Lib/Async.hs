module Lib.Async
  ( verifyCancelled
  ) where

import Control.Concurrent.Async
import qualified Control.Exception as E

verifyCancelled :: Async a -> IO (Either E.SomeException a)
verifyCancelled a = do
  cancel a
  waitCatch a
