{-# LANGUAGE OverloadedStrings #-}
module Lib.Regex
  ( TDFABS.compile
  , TDFABS.execute, TDFA.MatchArray
  , TDFABS.regexec
  , TDFA.Regex
  , defaultCompOption
  , defaultExecOption
  , defaultCompile
  , escape
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.TDFA.ByteString as TDFABS

defaultCompile :: ByteString -> Either String TDFA.Regex
defaultCompile = TDFABS.compile defaultCompOption defaultExecOption

defaultCompOption :: TDFA.CompOption
defaultCompOption = TDFA.CompOption
  { TDFA.caseSensitive = False
  , TDFA.multiline = False
  , TDFA.rightAssoc = False
  , TDFA.newSyntax = False
  , TDFA.lastStarGreedy = False
  }

defaultExecOption :: TDFA.ExecOption
defaultExecOption = TDFA.ExecOption
  { TDFA.captureGroups = True
  }

regexChars :: ByteString
regexChars = "\\|()[{^$*+?."

escapeChar :: Char -> ByteString
escapeChar x
  | x `BS8.elem` regexChars = BS8.pack ['\\', x]
  | otherwise = BS8.pack [x]

escape :: ByteString -> ByteString
escape = BS8.concatMap escapeChar
