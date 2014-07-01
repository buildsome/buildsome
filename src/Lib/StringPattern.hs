-- | The notion of a string pattern: abc%def can be matched or plugged
-- with a match
{-# LANGUAGE DeriveGeneric #-}
module Lib.StringPattern
  ( StringPattern(..), fromString
  , Match(..), match, plug
  ) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Lib.ByteString (unprefixed, unsuffixedWildCard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

data StringPattern = StringPattern
  { stringPatternPrefix :: ByteString
  , stringPatternSuffix :: ByteString
  } deriving (Show, Generic)
instance Binary StringPattern

data Match = Match
  { matchPlaceHolder :: ByteString -- which value % took in this match
  , matchWildcard :: Maybe ByteString -- which value the optional * took
  } deriving (Show, Generic)
instance Binary Match

match :: StringPattern -> ByteString -> Maybe Match
match (StringPattern prefix suffix) string =
  case unprefixed prefix string of
    Nothing -> Nothing
    Just m ->
      case unsuffixedWildCard suffix m of
        (Nothing, _) -> Nothing
        (Just m2, w) -> Just $ Match m2 w

plug :: Match -> StringPattern -> ByteString
plug (Match component Nothing) (StringPattern prefix suffix) =
  prefix <> component <> suffix
plug (Match component (Just wildcard)) (StringPattern prefix suffix) =
  prefix <> component <> (let (a, b) = BS.breakSubstring (BS8.pack "*") suffix in
      if BS.null b then suffix else a <> wildcard <> BS.drop 1 b)

fromString :: Char -> ByteString -> Maybe StringPattern
fromString splitter pattern =
  case BS8.split splitter pattern of
  [] -> Nothing
  [_] -> Nothing
  [prefix, suffix] ->
    Just StringPattern
      { stringPatternPrefix = prefix
      , stringPatternSuffix = suffix
      }
  _ -> error $ "Too many % in pattern: " ++ show pattern
