module Lib.Fresh
  ( Fresh, new
  , next
  ) where


import Prelude.Compat

import Data.IORef

newtype Fresh a = Fresh (IORef a)

{-# INLINE new #-}
new :: a -> IO (Fresh a)
new x = Fresh <$> newIORef x

{-# INLINE next #-}
next :: Enum a => Fresh a -> IO a
next (Fresh ref) = atomicModifyIORef' ref $ \old -> (succ old, old)
