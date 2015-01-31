module Lib.SyncMap
       (syncInsert)
       where

import           Control.Concurrent.MVar (newEmptyMVar, MVar(..), readMVar)
import           Control.Monad (join)
import           Data.IORef (atomicModifyIORef, IORef(..))

import qualified Control.Exception as E
import qualified Data.Map.Strict as M

syncInsert :: Ord k => IORef (M.Map k (MVar b)) -> k -> (MVar b -> IO b) -> IO b
syncInsert refMap k action =
  do newMVar <- newEmptyMVar
     join $ atomicModifyIORef refMap $
       \oldMap ->
        case M.lookup k oldMap of
         Just mvar -> (oldMap, readMVar mvar)
         Nothing -> (M.insert k newMVar oldMap, action newMVar)
