module Lib.Printer
  ( Id, idStr
  , Printer, new
  , putStrLn
  , printWrap
  ) where

import Prelude hiding (putStrLn)
import qualified Prelude

import Control.Applicative ((<$>))
import Data.IORef
import Data.List (intercalate)
import Text.Printf (printf)
import qualified Control.Exception as E

type Id = Int

data Printer = Printer
  { _printerId :: Id
  , printerIndentLevelRef :: IORef Int
  }

new :: Int -> IO Printer
new pid = Printer pid <$> newIORef 0

prefixLines :: String -> String -> String
prefixLines prefix = intercalate "\n" . map (prefix ++) . lines

idStr :: Id -> String
idStr = printf "%03d"

putStrLn :: Printer -> String -> IO ()
putStrLn (Printer pid indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = idStr pid ++ ": " ++ concat (replicate indentLevel "  ")
  Prelude.putStrLn $ prefixLines prefix str

onException :: IO a -> (E.SomeException -> IO ()) -> IO a
onException act f = act `E.catch` \e -> f e >> E.throwIO e

printWrap :: Printer -> String -> IO a -> IO a
printWrap printer str body = do
  putStrLn printer before
  res <-
    wrappedBody `onException` \e -> putStrLn printer $ after $ "EXCEPTION: " ++ show e
  putStrLn printer $ after "OK"
  return res
  where
    indentLevel  = printerIndentLevelRef printer
    addIndent d  = modifyIORef indentLevel (+d)
    wrappedBody  = E.bracket_ (addIndent 1) (addIndent (-1)) body
    before       = unwords ["{", str]
    after suffix = unwords ["}", str, suffix]
