module Buildsome.Color
  ( Scheme(..), defaultScheme, nonColorScheme, schemeForTerminal
  ) where

import Lib.ColorText (ColorText(..), withAttr)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
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
  }

defaultScheme :: Scheme
defaultScheme = Scheme
  { cWarning = withAttr [fgColor Vivid Yellow]
  , cError = withAttr [fgColor Vivid Red]
  , cTarget = withAttr [fgColor Vivid Cyan]
  , cPath = withAttr [fgColor Dull Cyan]
  , cTiming = withAttr [fgColor Vivid Blue]
  , cSuccess = withAttr [fgColor Vivid Green]
  , cCommand = withAttr [fgColor Dull White]
  , cStdout = withAttr [fgColor Dull Green]
  , cStderr = withAttr [fgColor Dull Red]
  }

nonColorScheme :: Scheme
nonColorScheme = Scheme
  { cWarning = id
  , cError = id
  , cTarget = id
  , cPath = id
  , cTiming = id
  , cSuccess = id
  , cCommand = id
  , cStdout = id
  , cStderr = id
  }

schemeForTerminal :: IO Scheme
schemeForTerminal = do
  isTty <- queryTerminal stdOutput
  return $
    if isTty
    then defaultScheme
    else nonColorScheme