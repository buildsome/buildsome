module Lib.AsyncContext
  ( new, AsyncContext
  , spawn
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.IntMap (IntMap)
import Lib.Fresh (Fresh)
import qualified Control.Exception as E
import qualified Data.IntMap as IntMap
import qualified Lib.Fresh as Fresh

data AsyncContext = AsyncContext
  { _ctxFreshNames :: Fresh Int
  , _ctxCancelActions :: MVar (IntMap (IO ()))
  }

new :: (AsyncContext -> IO a) -> IO a
new body = do
  freshNames <- Fresh.new 0
  cancelActionsVar <- newMVar IntMap.empty
  body (AsyncContext freshNames cancelActionsVar) `E.finally` do
    cancelActions <- readMVar cancelActionsVar
    sequence_ $ IntMap.elems cancelActions

spawn :: AsyncContext -> IO a -> IO (Async a)
spawn (AsyncContext freshNames cancelActionsVar) act = do
  name <- Fresh.next freshNames
  modifyMVarMasked cancelActionsVar $ \cancelActions -> do
    actAsync <-
      async $
      act `E.finally` modifyMVar_ cancelActionsVar (return . IntMap.delete name)
    return
      ( IntMap.insert name (cancel actAsync) cancelActions
      , actAsync
      )
