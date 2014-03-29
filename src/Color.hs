{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Color
  ( warning, error, target, path, timing, success, command, stdout, stderr
  ) where

import Data.Monoid
import Data.String (IsString(..))
import Prelude hiding (error)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import qualified System.Console.ANSI as Console

type WrapText a = (Eq a, IsString a, Monoid a) => a -> a

wrapSGR :: [Console.SGR] -> WrapText a
wrapSGR sgrs x
  | mempty == x = mempty
  | otherwise = mconcat [fromString (Console.setSGRCode sgrs), x, fromString (Console.setSGRCode [])]

fgColor :: ColorIntensity -> Color -> Console.SGR
fgColor = Console.SetColor Console.Foreground

bgColor :: ColorIntensity -> Color -> Console.SGR
bgColor = Console.SetColor Console.Background

warning :: WrapText a
warning = wrapSGR [fgColor Vivid Yellow]

error :: WrapText a
error = wrapSGR [fgColor Vivid Red]

target :: WrapText a
target = wrapSGR [fgColor Vivid Cyan]

path :: WrapText a
path = wrapSGR [fgColor Dull Cyan]

timing :: WrapText a
timing = wrapSGR [fgColor Vivid Blue]

success :: WrapText a
success = wrapSGR [fgColor Vivid Green]

command :: WrapText a
command = wrapSGR [fgColor Dull White]

stdout :: WrapText a
stdout = wrapSGR [bgColor Dull Green]

stderr :: WrapText a
stderr = wrapSGR [bgColor Dull Red]
