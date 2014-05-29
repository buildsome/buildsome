-- | The notion of a string pattern: abc%def can be matched or plugged
-- with a match
module Lib.StringPattern
  ( StringPattern(..), fromString
  , Match(..), match, plug
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Lib.ByteString (unprefixed, unsuffixed)
import Lib.Regex (Regex)
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.Regex as Regex

newtype StringPattern = StringPattern
  { stringPattern :: Regex }
  deriving (Show)

newtype Match = Match
  { matchPlaceHolder :: ByteString -- which value % took in this match
  }

match :: StringPattern -> ByteString -> Maybe Match
match (StringPattern prefix suffix) string =
  Match <$> (unprefixed prefix string >>= unsuffixed suffix)

plug :: Match -> StringPattern -> ByteString
plug (Match component) (StringPattern prefix suffix) =
  prefix <> component <> suffix

toRegex :: Char -> ByteString -> ByteString
toRegex splitter = go
  where
    go 

fromString :: Char -> ByteString -> Maybe StringPattern
fromString splitter pattern =
  case BS8.split splitter pattern of
  [] -> Nothing
  [_] -> Nothing
  [prefix, suffix] ->
    Just StringPattern
      { stringPattern = Regex.defaultCompile $ toRegex splitter pattern
      }
  _ -> error $ "Too many % in pattern: " ++ show pattern
