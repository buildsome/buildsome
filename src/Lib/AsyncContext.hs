module Lib.AsyncContext
  ( new, AsyncContext
  , spawn
  ) where

import Control.Concurrent.Async
import Data.IORef
import Data.IntMap (IntMap)
import Lib.Exception (finally, onException)
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_)
import qualified Control.Exception as E
import qualified Data.IntMap as IntMap
import qualified Lib.Fresh as Fresh

data AsyncContext = AsyncContext
  { _ctxFreshNames :: Fresh Int
  , _ctxCancelActions :: IORef (IntMap (IO ()))
  }

new :: (AsyncContext -> IO a) -> IO a
new body = do
  freshNames <- Fresh.new 0
  cancelActionsVar <- newIORef IntMap.empty
  body (AsyncContext freshNames cancelActionsVar) `finally` do
    cancelActions <- atomicModifyIORef cancelActionsVar $ \x -> (error "Attempt to use AsyncContext when it is finalized", x)
    sequence_ $ IntMap.elems cancelActions

-- TODO: spawnUnmasked
spawn :: AsyncContext -> IO a -> IO (Async a)
spawn (AsyncContext freshNames cancelActionsVar) act = do
  name <- Fresh.next freshNames
  E.mask_ $ do
    actAsync <-
      asyncWithUnmask $
      \unmask -> unmask act
                 `onException` atomicModifyIORef'_ cancelActionsVar (IntMap.delete name)
    atomicModifyIORef'_ cancelActionsVar $ IntMap.insert name (cancel actAsync)
    return actAsync
