module Lib.BuildId
  ( BuildId, new
  ) where

import Control.Applicative ((<$>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

newtype BuildId = BuildId String

new :: IO BuildId
new = BuildId . formatTime defaultTimeLocale "%y%m%d-%H%M%S%Q" <$> getCurrentTime
