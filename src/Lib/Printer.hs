{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}
module Lib.Printer
  ( Id, idStr
  , Printable
  , Printer, new, newFrom, render
  , printStrLn, rawPrintStrLn
  , printWrap
  , ColorScheme(..)
  , rawPrintWrap, rawPrinterWrap
  , putLn
  ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Monoid
import Data.String (IsString(..))
import Lib.ColorText (ColorText)
import Lib.Exception (onExceptionWith, bracket_)
import Prelude hiding (lines, putStrLn)
import Text.Printf (printf)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified GHC.IO.Exception as G
import qualified Lib.ColorText as ColorText
import qualified Prelude
import qualified System.IO as IO

ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished act = do
  res <- E.try act
  case res of
    Right x -> return x
    Left e@(G.IOError { G.ioe_type = t }) ->
      unless (t == G.ResourceVanished) $ E.throwIO e

wrapOutputCall :: IO () -> IO ()
wrapOutputCall = E.uninterruptibleMask_ . ignoreResourceVanished

putLn :: IO.Handle -> String -> IO ()
putLn handle = wrapOutputCall . IO.hPutStrLn handle

type Id = Int

class (IsString p, Monoid p) => Printable p where
  intercalate :: p -> [p] -> p
  lines :: p -> [p]
  putStrLn :: (ColorText -> ByteString) -> p -> IO ()

instance Printable String where
  intercalate = List.intercalate
  lines = List.lines
  putStrLn _ = Prelude.putStrLn
  {-# INLINE intercalate #-}
  {-# INLINE lines #-}
  {-# INLINE putStrLn #-}

instance Printable ByteString where
  intercalate = BS8.intercalate
  lines = BS8.lines
  putStrLn _ = BS8.putStrLn
  {-# INLINE intercalate #-}
  {-# INLINE lines #-}
  {-# INLINE putStrLn #-}

instance Printable ColorText where
  intercalate = ColorText.intercalate
  lines = ColorText.lines
  putStrLn toBS = BS8.putStrLn . toBS
  {-# INLINE intercalate #-}
  {-# INLINE lines #-}
  {-# INLINE putStrLn #-}

data Printer = Printer
  { _printerId :: Id
  , printerRender :: ColorText -> ByteString
  , printerIndentLevelRef :: IORef Int
  }

render :: Printer -> ColorText -> ByteString
render = printerRender

{-# INLINE new #-}
new :: (ColorText -> ByteString) -> Id -> IO Printer
new toBS pid = Printer pid toBS <$> newIORef 0

{-# INLINE newFrom #-}
newFrom :: Printer -> Id -> IO Printer
newFrom (Printer _id toBS indentRef) pid =
  Printer pid toBS <$> (newIORef =<< readIORef indentRef)

{-# INLINE idStr #-}
idStr :: IsString str => Id -> str
idStr = fromString . printf "T%03d"

{-# INLINE printStrLn #-}
printStrLn :: Printable str => Printer -> str -> IO ()
printStrLn printer@(Printer pid _ indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = idStr pid <> " " <> mconcat (replicate indentLevel "  ")
  rawPrintStrLn printer $ intercalate "\n" $ map (prefix <>) $ lines str

{-# INLINE rawPrintStrLn #-}
rawPrintStrLn :: Printable str => Printer -> str -> IO ()
rawPrintStrLn (Printer _ toBS _) = wrapOutputCall . putStrLn toBS

data ColorScheme = ColorScheme
  { cException :: ColorText -> ColorText
  , cOk :: ColorText -> ColorText
  }

{-# INLINE printWrap #-}
printWrap :: ColorScheme -> Printer -> ColorText -> ColorText -> IO a -> IO a
printWrap ColorScheme{..} printer str entryMsg body = do
  printStrLn printer before
  res <-
    wrappedBody `onExceptionWith` \e ->
    printStrLn printer $ after $ cException $
    "EXCEPTION: " <> (fromString . concat . take 1 . lines . show) e
  printStrLn printer $ after $ cOk "OK"
  return res
  where
    indentLevel  = printerIndentLevelRef printer
    addIndent d  = atomicModifyIORef' indentLevel $ \old -> (old+d, ())
    wrappedBody  = bracket_ (addIndent 1) (addIndent (-1)) body
    before       = mconcat ["{ ", str, " ", entryMsg]
    after suffix = mconcat ["} ", str, " ", suffix]

{-# INLINE rawPrintWrap #-}
rawPrintWrap :: String -> IO a -> IO a
rawPrintWrap str = bracket_ (putLn IO.stdout (str ++ "{")) (putLn IO.stdout (str ++ "}"))

{-# INLINE rawPrinterWrap #-}
rawPrinterWrap :: Printer -> String -> IO a -> IO a
rawPrinterWrap printer str =
  bracket_ (printStrLn printer (str ++ "{")) (printStrLn printer (str ++ "}"))
