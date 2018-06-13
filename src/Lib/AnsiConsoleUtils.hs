{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib.AnsiConsoleUtils () where

import           Data.Binary (Binary(..))
import qualified Data.Colour.SRGB as Colour
import           GHC.Generics (Generic)
import qualified System.Console.ANSI as Console

import           Prelude.Compat

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

deriving instance Generic (Colour.RGB a)
instance Binary a => Binary (Colour.RGB a)

instance (Ord a, Floating a, RealFrac a) => Binary (Colour.Colour a) where
    get = Colour.sRGB24 <$> get <*> get <*> get
    put = put . Colour.toSRGB24
