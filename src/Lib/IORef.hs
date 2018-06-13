module Lib.IORef (atomicModifyIORef_, atomicModifyIORef'_) where

import Data.IORef

import Prelude.Compat

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ioref f = atomicModifyIORef ioref (\x -> (f x, ()))

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ioref f = atomicModifyIORef' ioref (\x -> (f x, ()))
