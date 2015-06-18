{-# LANGUAGE DeriveGeneric #-}
module Buildsome.BuildId
  ( BuildId(..), new
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics (Generic)

newtype BuildId = BuildId String
  deriving (Show, Generic)

instance Binary BuildId

new :: IO BuildId
new = BuildId . formatTime defaultTimeLocale "%y%m%d-%H%M%S%Q" <$> getCurrentTime
