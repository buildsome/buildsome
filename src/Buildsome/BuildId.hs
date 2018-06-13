{-# LANGUAGE CPP #-}
module Buildsome.BuildId
  ( BuildId(..), new
  ) where


import Prelude.Compat

import Data.Binary (Binary)
import Data.Time.Clock (getCurrentTime)

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#else
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
#endif

import GHC.Generics (Generic)

newtype BuildId = BuildId String
  deriving (Show, Generic)

instance Binary BuildId

new :: IO BuildId
new = BuildId . formatTime defaultTimeLocale "%y%m%d-%H%M%S%Q" <$> getCurrentTime
