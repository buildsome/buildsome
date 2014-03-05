{-# LANGUAGE DeriveGeneric #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , printStdouts
  ) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS

data StdOutputs = StdOutputs
  { _stdOut :: ByteString
  , _stdErr :: ByteString
  } deriving (Generic, Show)
instance Binary StdOutputs

-- TODO: Where to place StdOutputs and printStdouts
printStdouts :: StdOutputs -> IO ()
printStdouts (StdOutputs stdout stderr) = do
  showOutput "STDOUT" stdout
  showOutput "STDERR" stderr
  where
    showOutput name bs
      | BS.null bs = return ()
      | otherwise = do
        putStrLn (name ++ ":")
        BS.putStr bs
