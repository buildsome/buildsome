{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , str
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

guaranteeTrailingNewline :: ByteString -> ByteString
guaranteeTrailingNewline bs
  | "\n" `BS8.isSuffixOf` bs = bs
  | otherwise = bs <> "\n"

-- Always ends with a newline
str :: ByteString -> StdOutputs -> ByteString
str strLabel (StdOutputs stdout stderr)
  | BS8.null stdout && BS8.null stderr = mconcat ["(", strLabel, "): no outputs\n"]
  | otherwise = mconcat
  [ "(", strLabel, ")\n"
  , showOutput "STDOUT" stdout
  , showOutput "STDERR" stderr
  ]
  where
    showOutput name bs
      | BS8.null bs = ""
      | otherwise = mconcat [name, ":\n", guaranteeTrailingNewline bs]
