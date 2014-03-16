module Lib.ShowBytes (showBytes) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

showBytes :: Integer -> String
showBytes count =
  fromMaybe (show count ++ " bytes") $
  f 50 "EB" <|>
  f 40 "TB" <|>
  f 30 "GB" <|>
  f 20 "MB" <|>
  f 10 "KB"
  where
    f :: Int -> String -> Maybe String
    f power name
      | count >= p = Just $ printf "%.2f%s" (fromIntegral count / fromIntegral p :: Double) name
      | otherwise = Nothing
      where
        p = (2 :: Integer) ^ power
