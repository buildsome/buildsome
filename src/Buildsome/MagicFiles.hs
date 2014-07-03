{-# LANGUAGE OverloadedStrings #-}
module Buildsome.MagicFiles
  ( inputIgnored, outputIgnored
  ) where

import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Data.ByteString.Char8 as BS8

specialFile :: FilePath -> Bool
specialFile path = any (`BS8.isPrefixOf` path) ["/dev", "/proc", "/sys", "/var/folders"]

inputIgnored :: FilePath -> Bool
inputIgnored = specialFile

outputIgnored :: FilePath -> Bool
outputIgnored = specialFile
