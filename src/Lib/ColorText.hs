{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.ColorText
  ( ColorText(..), normalize, simple
  , render, renderStr, stripColors
  , withAttr
  , intercalate, lines, putStrLn, singleton
  ) where

import Prelude.Compat hiding (putStrLn, lines)

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Lib.AnsiConsoleUtils ()
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified System.Console.ANSI as Console

newtype ColorText = ColorText { colorTextPairs :: [([Console.SGR], ByteString)] }
  deriving (Semigroup, Monoid, Show, Generic)
instance Binary ColorText

{-# INLINE onFirst #-}
onFirst :: (a -> a') -> (a, b) -> (a', b)
onFirst f (x, y) = (f x, y)

withAttr :: [Console.SGR] -> ColorText -> ColorText
withAttr sgrs (ColorText pairs) = ColorText $ (map . onFirst) (sgrs++) pairs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = List.groupBy ((==) `on` f)

normalize :: ColorText -> ColorText
normalize = ColorText . map concatGroup . groupOn fst . filter (not . BS8.null . snd) . colorTextPairs
  where
    concatGroup items@((attrs, _):_) = (attrs, BS8.concat (map snd items))
    concatGroup [] = error "groupOn yielded empty group!"

instance Eq ColorText where
  (==) = (==) `on` (colorTextPairs . normalize)

simple :: ByteString -> ColorText
simple x = ColorText [([], x)]

instance IsString ColorText where
  fromString = simple . fromString

putStrLn :: ColorText -> IO ()
putStrLn = BS8.putStrLn . render

renderStr :: ColorText -> String
renderStr = BS8.unpack . render

render :: ColorText -> ByteString
render = go [] . colorTextPairs
  where
    go [] [] = ""
    go (_:_) [] = fromString (Console.setSGRCode [])
    go curSgrs ((sgrs, x):rest) = colorCode <> x <> go sgrs rest
      where
        colorCode
          | curSgrs /= sgrs = fromString (Console.setSGRCode sgrs)
          | otherwise = ""

stripColors :: ColorText -> ByteString
stripColors = mconcat . map snd . colorTextPairs

intercalate :: Monoid m => m -> [m] -> m
intercalate x = mconcat . List.intersperse x

singleton :: [Console.SGR] -> ByteString -> ColorText
singleton attrs text = ColorText [(attrs, text)]

lines :: ColorText -> [ColorText]
lines (ColorText pairs) =
  foldr combine [] linedPairs
  where
    -- For each attr, hold a list of lines with that attr:
    linedPairs = (map . fmap) (BS8.split '\n') pairs

    -- combine each (attrs, list of lines) with the rest of the ColorText
    combine (_, []) restLines = restLines
    combine (attrs, ls@(_:_)) (ColorText restLinePairs:restLines) =
      map (singleton attrs) (init ls) ++
      (ColorText ((attrs, last ls) : restLinePairs) : restLines)
    combine (attrs, ls@(_:_)) [] = map (singleton attrs) ls
