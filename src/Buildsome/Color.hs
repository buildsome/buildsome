module Buildsome.Color
  ( Scheme(..), scheme
  ) where

import Lib.ColorText (ColorText(..), withAttr)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import qualified Lib.Printer as Printer
import qualified System.Console.ANSI as Console

fgColor :: ColorIntensity -> Color -> Console.SGR
fgColor = Console.SetColor Console.Foreground

-- bgColor :: ColorIntensity -> Color -> Console.SGR
-- bgColor = Console.SetColor Console.Background

data Scheme = Scheme
  { cWarning :: ColorText -> ColorText
  , cError :: ColorText -> ColorText
  , cTarget :: ColorText -> ColorText
  , cPath :: ColorText -> ColorText
  , cTiming :: ColorText -> ColorText
  , cSuccess :: ColorText -> ColorText
  , cCommand :: ColorText -> ColorText
  , cStdout :: ColorText -> ColorText
  , cStderr :: ColorText -> ColorText
  , cPrinter :: Printer.ColorScheme
  }

scheme :: Scheme
scheme = Scheme
  { cWarning = withAttr [fgColor Vivid Yellow]
  , cError   = withAttr [fgColor Vivid Red]
  , cTarget  = withAttr [fgColor Vivid Cyan]
  , cPath    = withAttr [fgColor Dull  Cyan]
  , cTiming  = withAttr [fgColor Vivid Blue]
  , cSuccess = withAttr [fgColor Vivid Green]
  , cCommand = withAttr [fgColor Dull  White]
  , cStdout  = withAttr [fgColor Dull  Green]
  , cStderr  = withAttr [fgColor Dull  Red]
  , cPrinter = Printer.ColorScheme
    { Printer.cException = withAttr [fgColor Vivid Red]
    , Printer.cOk        = withAttr [fgColor Vivid Green]
    }
  }
