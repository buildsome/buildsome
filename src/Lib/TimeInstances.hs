{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, FlexibleInstances, StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Prelude.Compat

import Data.Binary(Binary(..))
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Data.Fixed (Pico, Fixed(..), E12)

toPicos :: Pico -> Integer
fromPicos :: Integer -> Pico
#if __GLASGOW_HASKELL__ <= 706
toPicos = truncate . (*1e12)
fromPicos = (/1e12) . realToFrac
#else
toPicos (MkFixed x) = x
fromPicos = MkFixed
#endif
{-# INLINE toPicos #-}
{-# INLINE fromPicos #-}

instance Binary (Fixed E12) where
  get = fromPicos <$> get
  put = put . toPicos
  {-# INLINE get #-}
  {-# INLINE put #-}

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
