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
printStdouts :: String -> StdOutputs -> IO ()
printStdouts label (StdOutputs stdout stderr) = do
  showOutput ("STDOUT" ++ plabel) stdout
  showOutput ("STDERR" ++ plabel) stderr
  where
    plabel = "(" ++ label ++ ")"
    showOutput name bs
      | BS.null bs = return ()
      | otherwise = do
        putStrLn (name ++ ":")
        BS.putStr bs
