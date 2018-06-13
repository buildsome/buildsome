module Lib.Once
    ( once
    ) where

import Control.Monad (join)
import Data.IORef

import Prelude.Compat

once :: IO (IO a -> IO (Maybe a))
once = do
  doneRef <- newIORef False
  pure $ \act -> join $ atomicModifyIORef doneRef $ \x -> (True, f x act)
  where
    f False = fmap Just
    f True = const (pure Nothing)
