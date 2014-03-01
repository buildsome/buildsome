-- | The notion of a string pattern: abc%def can be matched or plugged
-- with a match
module Lib.StringPattern
  ( StringPattern(..), makePattern
  , Match(..), match, plug
  ) where

import Control.Applicative
import Data.List.Split (splitOn)
import Lib.List (unprefixed, unsuffixed)

data StringPattern = StringPattern
  { stringPatternPrefix :: String
  , stringPatternSuffix :: String
  } deriving (Show)

newtype Match = Match
  { matchPlaceHolder :: String -- which value % took in this match
  }

match :: StringPattern -> String -> Maybe Match
match (StringPattern prefix suffix) string = do
  Match <$> (unprefixed prefix string >>= unsuffixed suffix)

plug :: StringPattern -> Match -> String
plug (StringPattern prefix suffix) (Match component) =
  prefix ++ component ++ suffix

makePattern :: String -> Maybe StringPattern
makePattern pattern =
  case splitOn "%" pattern of
  [] -> Nothing
  [_] -> Nothing
  [prefix, suffix] ->
    Just StringPattern
      { stringPatternPrefix = prefix
      , stringPatternSuffix = suffix
      }
  _ -> error $ "Too many % in pattern: " ++ show pattern
