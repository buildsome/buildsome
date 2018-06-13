{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Data.Binary (Binary(..))
import Data.Fixed (Pico)
import Data.Time.Clock (NominalDiffTime, DiffTime)

import Prelude.Compat

{-# INLINE toPico #-}
toPico :: Real a => a -> Pico
toPico = realToFrac

{-# INLINE fromPico #-}
fromPico :: Fractional a => Pico -> a
fromPico = realToFrac

instance Binary NominalDiffTime where
  put = put . toPico
  get = fromPico <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}

instance Binary DiffTime where
  put = put . toPico
  get = fromPico <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}
