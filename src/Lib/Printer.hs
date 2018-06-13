{-# LANGUAGE FlexibleInstances, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Lib.Printer
  ( Id(..)
  , Printable
  , Printer, new, newFrom, render
  , printStrLn, rawPrintStrLn
  , printWrap
  , ColorScheme(..)
  , rawPrintWrap, rawPrinterWrap
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import qualified Data.List as List
import           Data.String (IsString(..))
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Exception (onExceptionWith, bracket_, putLn, swallowExceptions)
import qualified Lib.Show as Show
import qualified System.IO as IO
import           Text.Printf (printf)

import qualified Prelude.Compat as Prelude
import           Prelude.Compat hiding (lines, putStrLn)

newtype Id = Id Int
    deriving Enum

instance Show Id where
    {-# INLINE show #-}
    show (Id i) = printf "T%03d" i

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

{-# INLINE printStrLn #-}
printStrLn :: Printable str => Printer -> str -> IO ()
printStrLn printer@(Printer pid _ indentRef) str = do
  indentLevel <- readIORef indentRef
  let prefix = Show.show pid <> " " <> mconcat (replicate indentLevel "  ")
  rawPrintStrLn printer $ intercalate "\n" $ map (prefix <>) $ lines str

{-# INLINE rawPrintStrLn #-}
rawPrintStrLn :: Printable str => Printer -> str -> IO ()
rawPrintStrLn (Printer _ toBS _) = swallowExceptions . putStrLn toBS

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
  pure res
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
