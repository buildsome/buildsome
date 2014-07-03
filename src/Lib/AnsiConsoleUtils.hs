{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module Lib.AnsiConsoleUtils () where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified System.Console.ANSI as Console

deriving instance Generic Console.ConsoleIntensity
instance Binary Console.ConsoleIntensity

deriving instance Generic Console.Underlining
instance Binary Console.Underlining

deriving instance Generic Console.BlinkSpeed
instance Binary Console.BlinkSpeed

deriving instance Generic Console.Color
instance Binary Console.Color

deriving instance Generic Console.ColorIntensity
instance Binary Console.ColorIntensity

deriving instance Generic Console.ConsoleLayer
instance Binary Console.ConsoleLayer

deriving instance Generic Console.SGR
instance Binary Console.SGR
