module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
import Data.List (partition)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

data Target = Target
  { targetOutputPaths :: [FilePath]
  , targetInputHints :: [FilePath]
  , targetCmds :: [String]
  } deriving (Show)

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePhonies :: [FilePath]
  } deriving (Show)

isSeparator :: Char -> Bool
isSeparator x = P.isSpace x || x == ':'

word :: P.Parser BS.ByteString
word = P.takeWhile1 (not . isSeparator)

horizSpace :: P.Parser Char
horizSpace = P.satisfy $ \x -> x == ' ' || x == '\t'

lineWords :: P.Parser [BS.ByteString]
lineWords = many (many horizSpace *> word <* many horizSpace)

cmdLine :: P.Parser BS.ByteString
cmdLine =
  (P.char '\t' *> P.takeTill (== '\n')) <|>
  (BS.empty <$ (many horizSpace *> P.char '\n'))

target :: P.Parser Target
target = do
  outputPaths <- lineWords
  _ <- P.char ':'
  inputPaths <- lineWords
  _ <- P.char '\n'
  cmdLines <- filter (not . BS.null) <$> many cmdLine
  let unpack = map BS.unpack
  return Target
    { targetOutputPaths = unpack outputPaths
    , targetInputHints = unpack inputPaths
    , targetCmds = unpack cmdLines
    }

mkMakefile :: [Target] -> Makefile
mkMakefile targets
  | not $ null $ concatMap targetCmds phonyTargets = error ".PHONY targets may not have commands!"
  | otherwise =
    Makefile
    { makefileTargets = regularTargets
    , makefilePhonies = concatMap targetInputHints phonyTargets
    }
  where
    (phonyTargets, regularTargets) = partition ((== [".PHONY"]) . targetOutputPaths) targets

makefileParser :: P.Parser Makefile
makefileParser =
  (mkMakefile <$> many target) <* P.endOfInput
