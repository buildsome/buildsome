module Lib.IORef (atomicModifyIORef_) where

import Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ioref f = atomicModifyIORef ioref (\x -> (f x, ()))
