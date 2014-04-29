module Buildsome.Color
  ( warning, error, target, path, timing, success, command, stdout, stderr
  ) where

-- cannot hide specific Prelude names when not using any prelude names
-- without getting a warning
import Prelude()
import Lib.ColorText (ColorText(..), withAttr)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import qualified System.Console.ANSI as Console

fgColor :: ColorIntensity -> Color -> Console.SGR
fgColor = Console.SetColor Console.Foreground

-- bgColor :: ColorIntensity -> Color -> Console.SGR
-- bgColor = Console.SetColor Console.Background

warning :: ColorText -> ColorText
warning = withAttr [fgColor Vivid Yellow]

error :: ColorText -> ColorText
error = withAttr [fgColor Vivid Red]

target :: ColorText -> ColorText
target = withAttr [fgColor Vivid Cyan]

path :: ColorText -> ColorText
path = withAttr [fgColor Dull Cyan]

timing :: ColorText -> ColorText
timing = withAttr [fgColor Vivid Blue]

success :: ColorText -> ColorText
success = withAttr [fgColor Vivid Green]

command :: ColorText -> ColorText
command = withAttr [fgColor Dull White]

stdout :: ColorText -> ColorText
stdout = withAttr [fgColor Dull Green]

stderr :: ColorText -> ColorText
stderr = withAttr [fgColor Dull Red]
