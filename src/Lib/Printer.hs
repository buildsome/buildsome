{-# LANGUAGE OverloadedStrings #-}
module Lib.Printer
  ( Id, idStr
  , Printer, new, newFrom
  , putStrLn, bsPutStrLn
  , printWrap
  ) where

import Prelude hiding (putStrLn)
import qualified Prelude

import Control.Applicative ((<$>))
import Data.IORef
import Data.List (intercalate)
import Data.Monoid
import Text.Printf (printf)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8

type Id = Int

data Printer = Printer
  { _printerId :: Id
  , printerIndentLevelRef :: IORef Int
  }

new :: Id -> IO Printer
new pid = Printer pid <$> newIORef 0

newFrom :: Printer -> Id -> IO Printer
newFrom (Printer _id indentRef) pid = do
  Printer pid <$> (newIORef =<< readIORef indentRef)


idStr :: Id -> String
idStr = printf "T%03d"

putStrLn :: Printer -> String -> IO ()
putStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = idStr pid <> " " <> concat (replicate indentLevel "  ")
  Prelude.putStrLn $ prefixLines prefix str
  where
    prefixLines prefix = intercalate "\n" . map (prefix <>) . lines

bsPutStrLn :: Printer -> BS8.ByteString -> IO ()
bsPutStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = BS8.pack (idStr pid) <> " " <> BS8.concat (replicate indentLevel "  ")
  BS8.putStrLn $ prefixLines prefix str
  where
    prefixLines prefix = BS8.intercalate "\n" . map (prefix <>) . BS8.lines

onException :: IO a -> (E.SomeException -> IO ()) -> IO a
onException act f = act `E.catch` \e -> f e >> E.throwIO e

printWrap :: Printer -> String -> IO a -> IO a
printWrap printer str body = do
  putStrLn printer before
  res <-
    wrappedBody `onException` \e -> putStrLn printer $ after $ "EXCEPTION: " <> show e
  putStrLn printer $ after "OK"
  return res
  where
    indentLevel  = printerIndentLevelRef printer
    addIndent d  = modifyIORef indentLevel (+d)
    wrappedBody  = E.bracket_ (addIndent 1) (addIndent (-1)) body
    before       = unwords ["{", str]
    after suffix = unwords ["}", str, suffix]
