{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..))
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Data.Fixed (Pico, Fixed(..))
import GHC.Generics (Generic)

deriving instance Generic (Fixed a)
instance Binary (Fixed a)

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
