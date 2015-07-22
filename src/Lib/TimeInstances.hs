{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, FlexibleInstances, StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Data.Binary (Binary(..))
import Data.Fixed (Fixed(..))
import Data.Time.Clock (NominalDiffTime, DiffTime)
import GHC.Generics (Generic)

import Prelude.Compat

deriving instance Generic (Fixed a)
instance Binary (Fixed a)

instance Binary NominalDiffTime where
  put = put . toRational
  get = fromRational <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}

instance Binary DiffTime where
  put = put . toRational
  get = fromRational <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}
