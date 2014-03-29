{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , str
  ) where

import Data.Binary (Binary)
import Data.Monoid
import Data.String (IsString)
import GHC.Generics (Generic)

data StdOutputs a = StdOutputs
  { _stdOut :: a
  , _stdErr :: a
  } deriving (Generic, Show)
instance Binary a => Binary (StdOutputs a)

str :: (Eq a, Monoid a, IsString a) => a -> StdOutputs a -> Maybe a
str strLabel (StdOutputs stdout stderr)
  | mempty == stdout && mempty == stderr = Nothing
  | otherwise = Just $ mconcat
  [ strLabel
  , showOutput "STDOUT" stdout
  , showOutput "STDERR" stderr
  ]
  where
    showOutput name bs
      | mempty == bs = ""
      | otherwise = mconcat ["\n", name, ":\n", bs]
