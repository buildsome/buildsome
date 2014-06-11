{-# LANGUAGE OverloadedStrings #-}
module Buildsome.Print
  ( targetShow, targetWrap, targetTiming
  , warn, posMessage
  , cmd, targetStdOutputs
  , delimitMultiline
  , outputsStr
  ) where

import Buildsome.Db (Reason)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String (IsString(..))
import Lib.ByteString (chopTrailingNewline)
import Lib.ColorText (ColorText)
import Lib.Makefile (TargetType(..), Target)
import Lib.Parsec (showPos)
import Lib.Printer (Printer, printStrLn)
import Lib.Show (show)
import Lib.StdOutputs (StdOutputs(..))
import Prelude hiding (show)
import Text.Parsec (SourcePos)
import qualified Buildsome.Color as Color
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.ColorText as ColorText
import qualified Lib.Printer as Printer
import qualified Lib.StdOutputs as StdOutputs

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

targetShow :: Show a => a -> ColorText
targetShow = Color.target . show

posMessage :: SourcePos -> ColorText -> IO ()
posMessage pos msg =
  ColorText.putStrLn $ mconcat [fromString (showPos pos), ": ", msg]

warn :: SourcePos -> ColorText -> IO ()
warn pos str =
  posMessage pos $ Color.warning $ "WARNING: " <> str

targetWrap :: Printer -> Reason -> Target -> ColorText -> IO a -> IO a
targetWrap printer reason target str =
  Printer.printWrap printer (targetShow (targetOutputs target)) $ mconcat
  [str, " (", fromBytestring8 reason, ")"]

targetTiming :: Show a => Printer -> ColorText -> a -> IO ()
targetTiming printer str selfTime =
  printStrLn printer $ ColorText.render $
    "Build (" <> str <> ") took " <> Color.timing (show selfTime <> " seconds")

colorStdOutputs :: StdOutputs ByteString -> StdOutputs ColorText
colorStdOutputs (StdOutputs out err) =
  StdOutputs
  (colorize Color.stdout out)
  (colorize Color.stderr err)
  where
    colorize f = f . fromBytestring8 . chopTrailingNewline

outputsStr :: ColorText -> StdOutputs ByteString -> Maybe ColorText
outputsStr label = StdOutputs.str label . colorStdOutputs

targetStdOutputs :: Target -> StdOutputs ByteString -> IO ()
targetStdOutputs target stdOutputs =
  maybe (return ()) ColorText.putStrLn $
  outputsStr (targetShow (targetOutputs target)) stdOutputs

cmd :: Printer -> Target -> IO ()
cmd printer target =
  unless (BS8.null cmds) $ printStrLn printer $ delimitMultiline $
  ColorText.render $ Color.command $ fromBytestring8 cmds
  where
    cmds = targetCmds target

delimitMultiline :: ByteString -> ByteString
delimitMultiline xs
  | '\n' `BS8.notElem` x = x
  | otherwise = multilineDelimiter <> x <> multilineDelimiter
  where
    x = chopTrailingNewline xs
    multilineDelimiter = "\"\"\""
