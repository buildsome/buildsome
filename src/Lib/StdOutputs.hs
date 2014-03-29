{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , str
  ) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Monoid
import GHC.Generics (Generic)
import Lib.ByteString (chopTrailingNewline)
import qualified Data.ByteString.Char8 as BS8

data StdOutputs = StdOutputs
  { _stdOut :: ByteString
  , _stdErr :: ByteString
  } deriving (Generic, Show)
instance Binary StdOutputs

str :: ByteString -> StdOutputs -> Maybe ByteString
str strLabel (StdOutputs stdout stderr)
  | BS8.null stdout && BS8.null stderr = Nothing
  | otherwise = Just $ mconcat
  [ strLabel
  , showOutput "STDOUT" stdout
  , showOutput "STDERR" stderr
  ]
  where
    showOutput name bs
      | BS8.null bs = ""
      | otherwise = mconcat ["\n", name, ":\n", chopTrailingNewline bs]
