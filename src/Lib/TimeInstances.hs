{-# LANGUAGE CPP, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..))
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Data.Fixed (Pico, Fixed(..), HasResolution)

#if __GLASGOW_HASKELL__ <= 706
fromFixed :: HasResolution a => Fixed a -> Integer
fromFixed = truncate . (*1e12)
{-# INLINE fromFixed #-}
toFixed :: HasResolution a => Integer -> Fixed a
toFixed = (/1e12) . fromIntegral
{-# INLINE toFixed #-}
instance HasResolution a => Binary (Fixed a) where
  get = toFixed <$> get
  put = put . fromFixed
  {-# INLINE get #-}
  {-# INLINE put #-}
#else
import GHC.Generics (Generic)
deriving instance Generic (Fixed a)
instance Binary (Fixed a)
#endif

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
