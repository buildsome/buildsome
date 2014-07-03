{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Buildsome.Print
  ( targetWrap, targetTiming
  , warn, posMessage
  , replayCmd, executionCmd, targetStdOutputs
  , delimitMultiline
  , outputsStr
  , replay
  ) where

import Buildsome.Db (Reason)
import Buildsome.Opts (Verbosity(..), PrintOutputs(..), PrintCommands(..))
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid
import Data.String (IsString(..))
import Lib.ByteString (chopTrailingNewline)
import Lib.ColorText (ColorText)
import Lib.Exception (onException)
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

posMessage :: SourcePos -> ColorText -> IO ()
posMessage pos msg =
  ColorText.putStrLn $ mconcat [fromString (showPos pos), ": ", msg]

warn :: Color.Scheme -> SourcePos -> ColorText -> IO ()
warn Color.Scheme{..} pos str =
  posMessage pos $ cWarning $ "WARNING: " <> str

targetWrap :: Color.Scheme -> Printer -> Reason -> Target -> ColorText -> IO a -> IO a
targetWrap colors@Color.Scheme{..} printer reason target str =
  Printer.printWrap (Color.cPrinter colors) printer
  (cTarget (show (targetOutputs target))) $
  mconcat [str, " (", reason, ")"]

targetTiming :: Show a => Color.Scheme -> Printer -> ColorText -> a -> IO ()
targetTiming Color.Scheme{..} printer str selfTime =
  printStrLn printer $
    "Build (" <> str <> ") took " <> cTiming (show selfTime <> " seconds")

colorStdOutputs :: Color.Scheme -> StdOutputs ByteString -> StdOutputs ColorText
colorStdOutputs Color.Scheme{..} (StdOutputs out err) =
  StdOutputs
  (colorize cStdout out)
  (colorize cStderr err)
  where
    colorize f = f . fromBytestring8 . chopTrailingNewline

outputsStr :: Color.Scheme -> StdOutputs ByteString -> Maybe ColorText
outputsStr colors = StdOutputs.str . colorStdOutputs colors

targetStdOutputs :: Color.Scheme -> Target -> StdOutputs ByteString -> IO ()
targetStdOutputs colors _target stdOutputs =
  maybe (return ()) ColorText.putStrLn $ outputsStr colors stdOutputs

cmd :: Color.Scheme -> Printer -> Target -> IO ()
cmd Color.Scheme{..} printer target =
  unless (BS8.null cmds) $ printStrLn printer $ delimitMultiline $
  ColorText.render $ cCommand $ fromBytestring8 cmds
  where
    cmds = targetCmds target

replayCmd :: Color.Scheme -> PrintCommands -> Printer -> Target -> IO ()
replayCmd colors PrintCommandsForAll printer target = cmd colors printer target
replayCmd _      PrintCommandsForExecution _ _ = return ()
replayCmd _      PrintCommandsNever _ _ = return ()

executionCmd :: Color.Scheme -> PrintCommands -> Printer -> Target -> IO ()
executionCmd colors PrintCommandsForAll printer target = cmd colors printer target
executionCmd colors PrintCommandsForExecution printer target = cmd colors printer target
executionCmd _      PrintCommandsNever _ _ = return ()

delimitMultiline :: ByteString -> ByteString
delimitMultiline xs
  | '\n' `BS8.notElem` x = x
  | otherwise = multilineDelimiter <> x <> multilineDelimiter
  where
    x = chopTrailingNewline xs
    multilineDelimiter = "\"\"\""

replay :: Show a => Color.Scheme -> Printer -> Target -> StdOutputs ByteString -> Verbosity -> a -> IO () -> IO ()
replay colors@Color.Scheme{..} printer target stdOutputs verbosity selfTime action = do
  action `onException` \e -> do
    printStrLn printer $ "REPLAY for target " <> cTarget (show (targetOutputs target))
    cmd colors printer target
    targetStdOutputs colors target stdOutputs
    printStrLn printer $ cError $ "EXCEPTION: " <> show e
  when shouldPrint $ do
    printStrLn printer $ mconcat
      [ "REPLAY for target ", cTarget (show (targetOutputs target))
      , " (originally) took ", cTiming (show selfTime <> " seconds")
      , outputsHeader
      ]
    replayCmd colors (verbosityCommands verbosity) printer target
    targetStdOutputs colors target stdOutputs
  where
    shouldPrint =
      case verbosityOutputs verbosity of
      PrintOutputsAnyway -> True
      PrintOutputsNonEmpty -> not (StdOutputs.null stdOutputs)
      PrintOutputsIfStderr -> not (BS8.null (StdOutputs.stdErr stdOutputs))
    outputsHeader =
      case (BS8.null (StdOutputs.stdOut stdOutputs),
            BS8.null (StdOutputs.stdErr stdOutputs)) of
      (False, False) -> ", STDOUT/STDERR follow"
      (False, True)  -> ", STDOUT follows"
      (True, False)  -> ", STDERR follows"
      (True, True)   -> ""
