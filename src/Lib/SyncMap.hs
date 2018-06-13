{-# LANGUAGE TupleSections #-}
module Lib.SyncMap
    ( SyncMap
    , Inserted(..)
    , Result
    , toMap
    , tryInsert
    , insert
    , new
    ) where

import Prelude.Compat

import           Control.Concurrent.MVar
import qualified Control.Exception       as E
import           Control.Monad           (join)


import           Data.IORef
import qualified Data.Map                as M
import           Data.Map                (Map)

type Result = Either E.SomeException

newtype SyncMap k a = SyncMap (IORef (Map k (MVar (Result a))))

data Inserted = Inserted | Old


toMap :: SyncMap k a -> IO (Map k (Result a))
toMap (SyncMap ioRefMap) = readIORef ioRefMap >>= traverse readMVar

-- | Safely insert (or update) a value into a SyncMap.
--
-- Uses masking internally to ensure one of two outcomes:
--
-- 1. The map is updated: if the key is missing from the map, the given action is run, and the resulting value is inserted
-- 2. The map is not updated: if the key exists, returns the old value.
--
-- If an exception occurred during the synchronous update, it will be returned in the `Result`.
--
-- If blocking operations are used in the action, it is the user's
-- responsibility to use uninterruptibleMask or accept an async
-- exception as a result!
--
tryInsert :: Ord k => SyncMap k a -> k -> IO a -> IO (Inserted, Result a)
tryInsert (SyncMap refMap) key action =
    do  mvar <- newEmptyMVar
        let fillMVar x =
                do putMVar mvar x
                   return (Inserted, x)
        E.mask_ . join $
            atomicModifyIORef refMap $
            \oldMap ->
                case M.lookup key oldMap of
                    Just oldMVar -> (oldMap, (Old,) <$> readMVar oldMVar)
                    Nothing ->
                        ( M.insert key mvar oldMap
                        , E.try action >>= fillMVar
                        )

-- | Version of tryInsert that throws exceptions instead of returning them in a `Result`, and also discards the `Inserted` value
insert :: Ord k => SyncMap k a -> k -> IO a -> IO a
insert syncMap key action =
    do  (_, res) <- tryInsert syncMap key action
        either E.throwIO return res

new :: IO (SyncMap k v)
new = SyncMap <$> newIORef M.empty
