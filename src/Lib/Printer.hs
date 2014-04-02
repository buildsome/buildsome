{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Lib.Printer
  ( Id, idStr
  , Printable
  , Printer, new, newFrom
  , printStrLn
  , printWrap
  ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.IORef
import Data.Monoid
import Data.String (IsString(..))
import Lib.ColorText (ColorText)
import Prelude hiding (putStrLn, lines)
import Text.Printf (printf)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Lib.ColorText as ColorText
import qualified Prelude
import qualified System.Console.ANSI as Console

type Id = Int

class (IsString p, Monoid p) => Printable p where
  intercalate :: p -> [p] -> p
  lines :: p -> [p]
  putStrLn :: p -> IO ()

instance Printable String where
  intercalate = List.intercalate
  lines = List.lines
  putStrLn = putStrLn

instance Printable ByteString where
  intercalate = BS8.intercalate
  lines = BS8.lines
  putStrLn = BS8.putStrLn

instance Printable ColorText where
  intercalate = ColorText.intercalate
  lines = ColorText.lines
  putStrLn = ColorText.putStrLn

data Printer = Printer
  { _printerId :: Id
  , printerIndentLevelRef :: IORef Int
  }

{-# INLINE new #-}
new :: Id -> IO Printer
new pid = Printer pid <$> newIORef 0

{-# INLINE newFrom #-}
newFrom :: Printer -> Id -> IO Printer
newFrom (Printer _id indentRef) pid =
  Printer pid <$> (newIORef =<< readIORef indentRef)

{-# INLINE idStr #-}
idStr :: IsString str => Id -> str
idStr = fromString . printf "T%03d"

{-# INLINE printStrLn #-}
printStrLn :: Printable str => Printer -> str -> IO ()
printStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = idStr pid <> " " <> mconcat (replicate indentLevel "  ")
  putStrLn . intercalate "\n" . map (prefix <>) . lines $ str

{-# INLINE onException #-}
onException :: IO a -> (E.SomeException -> IO ()) -> IO a
onException act f = act `E.catch` \e -> f e >> E.throwIO e

exceptionTextAttrs :: [Console.SGR]
exceptionTextAttrs = [Console.SetColor Console.Foreground Console.Vivid Console.Red]

okTextAttrs :: [Console.SGR]
okTextAttrs = [Console.SetColor Console.Foreground Console.Vivid Console.Green]

{-# INLINE printWrap #-}
printWrap :: Printer -> ColorText -> IO a -> IO a
printWrap printer str body = do
  printStrLn printer before
  res <-
    wrappedBody `onException` \e ->
    printStrLn printer $ after $
    ColorText.withAttr exceptionTextAttrs $
    "EXCEPTION: " <> fromString ((concat . take 1 . lines . show) e)
  printStrLn printer $ after $
    ColorText.withAttr okTextAttrs "OK"
  return res
  where
    indentLevel  = printerIndentLevelRef printer
    addIndent d  = modifyIORef indentLevel (+d)
    wrappedBody  = E.bracket_ (addIndent 1) (addIndent (-1)) body
    before       = mconcat ["{ ", str]
    after suffix = mconcat ["} ", str, " ", suffix]
