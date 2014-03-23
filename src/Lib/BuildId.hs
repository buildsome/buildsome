{-# LANGUAGE DeriveGeneric #-}
module Lib.BuildId
  ( BuildId, new
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import GHC.Generics (Generic)
import System.Locale (defaultTimeLocale)

newtype BuildId = BuildId String
  deriving (Show, Generic)

instance Binary BuildId

new :: IO BuildId
new = BuildId . formatTime defaultTimeLocale "%y%m%d-%H%M%S%Q" <$> getCurrentTime
