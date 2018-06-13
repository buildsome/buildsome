module Lib.Once
    ( once
    ) where

import Control.Monad (join)
import Data.IORef

import Prelude.Compat

once :: IO (IO a -> IO (Maybe a))
once = do
  doneRef <- newIORef False
  return $ \act -> join $ atomicModifyIORef doneRef $ \x -> (True, f x act)
  where
    f False = fmap Just
    f True = const (return Nothing)
