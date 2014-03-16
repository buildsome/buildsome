{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , printStdouts
  ) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Monoid
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8

data StdOutputs = StdOutputs
  { _stdOut :: ByteString
  , _stdErr :: ByteString
  } deriving (Generic, Show)
instance Binary StdOutputs

-- TODO: Where to place StdOutputs and printStdouts
printStdouts :: String -> StdOutputs -> IO ()
printStdouts strLabel (StdOutputs stdout stderr) = do
  showOutput ("STDOUT" <> plabel) stdout
  showOutput ("STDERR" <> plabel) stderr
  where
    label = BS8.pack strLabel
    indent = BS8.intercalate "\n" . map ("  " <>) . BS8.lines
    plabel
      | '\n' `BS8.elem` label = "(\n" <> indent label <> "\n)"
      | otherwise = "(" <> label <> ")"
    showOutput name bs
      | BS8.null bs = return ()
      | otherwise = BS8.putStr $ (name <> ":\n") <> indent bs
