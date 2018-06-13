-- | The notion of a string pattern: abc%def can be matched or plugged
-- with a match
module Lib.StringPattern
    ( StringPattern, fromString, toString
    , Match, matchPlaceHolder1, matchPlaceHolder2, match, plug
    ) where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           GHC.Generics (Generic)
import           Lib.ByteString (unprefixed, unsuffixed, splitBS)

import           Prelude.Compat

-- String Pattern is one of:
-- 1. A%B
-- 2. A%B*C

-- A,B substrings always exist. C may or may not exist.

data StringPattern = StringPattern
    { _stringPatternA :: ByteString
    , _stringPatternB :: ByteString       -- After the %
    , _stringPatternC :: Maybe ByteString -- After the *, if * exists
    } deriving (Show, Generic)
instance Binary StringPattern
instance NFData StringPattern where rnf = genericRnf

data Match = Match
    { _matchPlaceHolder1 :: ByteString       -- which value % took in this match
    , _matchPlaceHolder2 :: Maybe ByteString -- which value the optional * took
    } deriving (Show, Generic)
instance Binary Match
instance NFData Match where rnf = genericRnf

matchPlaceHolder1 :: Match -> ByteString
matchPlaceHolder1 = _matchPlaceHolder1

matchPlaceHolder2 :: Match -> Maybe ByteString
matchPlaceHolder2 = _matchPlaceHolder2

match :: StringPattern -> ByteString -> Maybe Match
match (StringPattern a b Nothing) string =
    do
        afterA <- unprefixed a string
        placeHolder1 <- unsuffixed b afterA
        Just $ Match placeHolder1 Nothing
match (StringPattern a b (Just c)) string =
    do
        afterA <- unprefixed a string -- String from % to the end
        (placeHolder1, afterB)  <- splitBS b afterA
        placeHolder2 <- unsuffixed c afterB
        Just $ Match placeHolder1 $ Just placeHolder2

plug :: Match -> StringPattern -> Maybe ByteString
plug (Match placeHolder1 (Just placeHolder2)) (StringPattern a b (Just c)) =
    Just $ a <> placeHolder1 <> b <> placeHolder2 <> c
plug (Match placeHolder1 _) (StringPattern a b Nothing) =
    Just $ a <> placeHolder1 <> b
plug (Match _ Nothing) (StringPattern _ _ (Just _)) = Nothing

toString :: StringPattern -> ByteString
toString (StringPattern a b Nothing)  = a <> "%" <> b
toString (StringPattern a b (Just c)) = a <> "%" <> b <> "*" <> c

fromString :: ByteString -> Maybe StringPattern
fromString pat =
    case BS8.split '%' pat of
    [] -> Nothing
    [_] -> Nothing
    [a, afterA] ->
        Just $
        case BS8.split '*' afterA of
            [] -> noAsterisk
            [_] -> noAsterisk
            [b, c] -> StringPattern a b $ Just c
            _ -> error $ "Too many * in pattern: " ++ show pat
            where
                noAsterisk = StringPattern a afterA Nothing
    _ -> error $ "Too many % in pattern: " ++ show pat
