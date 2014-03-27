{-# LANGUAGE OverloadedStrings #-}
module Lib.Printer
  ( Id, idStr
  , Printer, new, newFrom
  , putStrLn, bsPutStrLn
  , printWrap, bsPrintWrap
  ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (intercalate)
import Data.Monoid
import Data.String (IsString(..))
import Prelude hiding (putStrLn)
import Text.Printf (printf)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Prelude

type Id = Int

data Printer = Printer
  { _printerId :: Id
  , printerIndentLevelRef :: IORef Int
  }

{-# INLINE new #-}
new :: Id -> IO Printer
new pid = Printer pid <$> newIORef 0

{-# INLINE newFrom #-}
newFrom :: Printer -> Id -> IO Printer
newFrom (Printer _id indentRef) pid = do
  Printer pid <$> (newIORef =<< readIORef indentRef)

{-# INLINE idStr #-}
idStr :: Id -> String
idStr = printf "T%03d"

{-# INLINE putStrLn #-}
putStrLn :: Printer -> String -> IO ()
putStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = idStr pid <> " " <> concat (replicate indentLevel "  ")
  Prelude.putStrLn $ prefixLines prefix str
  where
    prefixLines prefix = intercalate "\n" . map (prefix <>) . lines

{-# INLINE bsPutStrLn #-}
bsPutStrLn :: Printer -> ByteString -> IO ()
bsPutStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = BS8.pack (idStr pid) <> " " <> BS8.concat (replicate indentLevel "  ")
  BS8.putStrLn $ prefixLines prefix str
  where
    prefixLines prefix = BS8.intercalate "\n" . map (prefix <>) . BS8.lines

{-# INLINE onException #-}
onException :: IO a -> (E.SomeException -> IO ()) -> IO a
onException act f = act `E.catch` \e -> f e >> E.throwIO e

{-# INLINE printWrapWith #-}
printWrapWith ::
  (IsString s, Monoid s) =>
  (Printer -> s -> IO ()) ->
  Printer -> s -> IO a -> IO a
printWrapWith putLn printer str body = do
  putLn printer before
  res <-
    wrappedBody `onException` \e -> putLn printer $ after $ "EXCEPTION: " <> fromString (show e)
  putLn printer $ after "OK"
  return res
  where
    indentLevel  = printerIndentLevelRef printer
    addIndent d  = modifyIORef indentLevel (+d)
    wrappedBody  = E.bracket_ (addIndent 1) (addIndent (-1)) body
    before       = mconcat ["{ ", str]
    after suffix = mconcat ["} ", str, " ", suffix]

{-# INLINE printWrap #-}
printWrap :: Printer -> String -> IO a -> IO a
printWrap = printWrapWith putStrLn

{-# INLINE bsPrintWrap #-}
bsPrintWrap :: Printer -> ByteString -> IO a -> IO a
bsPrintWrap = printWrapWith bsPutStrLn
