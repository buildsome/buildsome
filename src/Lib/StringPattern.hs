-- | The notion of a string pattern: abc%def can be matched or plugged
-- with a match
{-# LANGUAGE DeriveGeneric #-}
module Lib.StringPattern
  ( StringPattern, fromString, toString
  , Match, matchPlaceHolder, match, plug
  ) where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
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
instance NFData StringPattern where rnf = genericRnf

data Match = Match
  { _matchPlaceHolder :: ByteString -- which value % took in this match
  , _matchWildcard :: Maybe ByteString -- which value the optional * took
  } deriving (Show, Generic)
instance Binary Match
instance NFData Match where rnf = genericRnf

matchPlaceHolder :: Match -> ByteString
matchPlaceHolder = _matchPlaceHolder

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

toString :: Char -> StringPattern -> ByteString
toString splitter (StringPattern prefix suffix) =
  prefix <> BS8.singleton splitter <> suffix

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
