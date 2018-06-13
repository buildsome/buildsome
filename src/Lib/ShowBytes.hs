module Lib.ShowBytes (showBytes) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String (IsString(..))
import Text.Printf (printf)

import Prelude.Compat

showBytes :: (Monoid str, IsString str) => Integer -> str
showBytes count =
  fromMaybe (fromString (show count) <> " bytes") $
  f 50 "EB" <|>
  f 40 "TB" <|>
  f 30 "GB" <|>
  f 20 "MB" <|>
  f 10 "KB"
  where
    f power name
      | count >= p =
        Just $ fromString (printf "%.2f" (fromIntegral count / fromIntegral p :: Double)) <> name
      | otherwise = Nothing
      where
        p = (2 :: Integer) ^ (power :: Int)
