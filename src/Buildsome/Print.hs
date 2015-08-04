{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Buildsome.Print
  ( targetWrap, targetTiming
  , warn
  , posText, posMessage
  , replayCmd, executionCmd, targetStdOutputs
  , buildsomeCreation
  , delimitMultiline
  , outputsStr
  , replay
  ) where

import Prelude.Compat hiding (show)

import qualified Buildsome.Color as Color
import           Buildsome.Db (Reason)
import           Buildsome.Opts (Verbosity(..), PrintOutputs(..), PrintCommands(..))
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import           Data.String (IsString(..))
import           Lib.ByteString (chopTrailingNewline)
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Exception (onExceptionWith)
import           Lib.Makefile (TargetType(..), Target)
import           Lib.Parsec (showPos)
import           Lib.Printer (Printer, printStrLn)
import qualified Lib.Printer as Printer
import           Lib.Show (show)
import           Lib.StdOutputs (StdOutputs(..))
import qualified Lib.StdOutputs as StdOutputs
import           Text.Parsec (SourcePos)

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

posText :: (Monoid s, IsString s) => SourcePos -> s
posText pos = mconcat [fromString (showPos pos), ": "]

posMessage :: Printer -> SourcePos -> ColorText -> IO ()
posMessage printer pos msg = Printer.rawPrintStrLn printer $ posText pos <> msg

warn :: Printer -> SourcePos -> ColorText -> IO ()
warn printer pos str =
  posMessage printer pos $ cWarning $ "WARNING: " <> str
  where
    Color.Scheme{..} = Color.scheme

targetWrap ::
  Printer -> Reason -> Target -> ColorText -> IO a -> IO a
targetWrap printer reason target str =
  Printer.printWrap cPrinter printer
  (cTarget (show (targetOutputs target))) $
  mconcat [str, " (", show reason, ")"]
  where
    Color.Scheme{..} = Color.scheme

targetTiming :: Show a => Printer -> ColorText -> a -> IO ()
targetTiming printer str selfTime =
  printStrLn printer $
    "Build (" <> str <> ") took " <> cTiming (show selfTime <> " seconds")
  where
    Color.Scheme{..} = Color.scheme

colorStdOutputs :: StdOutputs ByteString -> StdOutputs ColorText
colorStdOutputs (StdOutputs out err) =
  StdOutputs
  (colorize cStdout out)
  (colorize cStderr err)
  where
    colorize f = f . fromBytestring8 . chopTrailingNewline
    Color.Scheme{..} = Color.scheme

outputsStr :: StdOutputs ByteString -> Maybe ColorText
outputsStr = StdOutputs.str . colorStdOutputs

targetStdOutputs :: Printer -> Target -> StdOutputs ByteString -> IO ()
targetStdOutputs printer _target stdOutputs =
  maybe (return ()) (Printer.rawPrintStrLn printer) $ outputsStr stdOutputs

cmd :: Printer -> Target -> IO ()
cmd printer target =
  unless (BS8.null cmds) $ printStrLn printer $ cCommand $ fromBytestring8 $
  delimitMultiline cmds
  where
    cmds = targetCmds target
    Color.Scheme{..} = Color.scheme

replayCmd :: PrintCommands -> Printer -> Target -> IO ()
replayCmd PrintCommandsForAll printer target = cmd printer target
replayCmd PrintCommandsForExecution _ _ = return ()
replayCmd PrintCommandsNever _ _ = return ()

executionCmd :: PrintCommands -> Printer -> Target -> IO ()
executionCmd PrintCommandsForAll printer target = cmd printer target
executionCmd PrintCommandsForExecution printer target = cmd printer target
executionCmd PrintCommandsNever _ _ = return ()

delimitMultiline :: ByteString -> ByteString
delimitMultiline xs
  | '\n' `BS8.notElem` x = x
  | otherwise = multilineDelimiter <> x <> multilineDelimiter
  where
    x = chopTrailingNewline xs
    multilineDelimiter = "\"\"\""

replay :: Show a => Printer -> Target -> StdOutputs ByteString -> Reason -> Verbosity -> a -> IO () -> IO ()
replay printer target stdOutputs reason verbosity selfTime action = do
  action `onExceptionWith` \e -> do
    printStrLn printer header
    cmd printer target
    targetStdOutputs printer target stdOutputs
    printStrLn printer $ cError $ "EXCEPTION: " <> show e
  when shouldPrint $ do
    printStrLn printer $ mconcat
      [ header, " (originally) took ", cTiming (show selfTime <> " seconds")
      , outputsHeader
      ]
    replayCmd (verbosityCommands verbosity) printer target
    targetStdOutputs printer target stdOutputs
  where
    header = "REPLAY for target " <> cTarget (show (targetOutputs target)) <> " (" <> show reason <> ")"
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
    Color.Scheme{..} = Color.scheme

buildsomeCreation :: Show a => Printer -> ByteString -> [a] -> [a] -> Verbosity -> IO ()
buildsomeCreation printer version withs withouts verbosity
  | verbosityGeneral verbosity =
    printStrLn printer $ mconcat
    [ header
    , " --with ", show withs
    , " --without ", show withouts
    ]
  | null (withs ++ withouts) = return ()
  | otherwise =
    printStrLn printer $ mconcat
    [ header
    , if null withs    then "" else " --with "    <> show withs
    , if null withouts then "" else " --without " <> show withouts
    ]
  where
    header =
      "Buildsome " <> cTiming (ColorText.simple version) <> " invoked"
    Color.Scheme{..} = Color.scheme
