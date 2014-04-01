{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..))
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Foreign.C.Types (CTime(..))

instance Binary CTime where
  put (CTime x) = put x
  get = CTime <$> get

instance Binary NominalDiffTime where
  put = put . toRational
  get = fromRational <$> get

instance Binary DiffTime where
  put = put . toRational
  get = fromRational <$> get
