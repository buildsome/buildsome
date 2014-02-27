module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
import Control.Monad
import Data.List (partition)
import qualified Data.Char as C
import qualified Text.Parsec as P

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
isSeparator x = C.isSpace x || x == ':'

type Parser = P.Parsec String ()

word :: Parser String
word = some (P.satisfy (not . isSeparator))

horizSpace :: Parser Char
horizSpace = P.satisfy $ \x -> x == ' ' || x == '\t'

tillEndOfLine :: Parser String
tillEndOfLine = P.many (P.satisfy (/= '\n'))

comment :: Parser ()
comment = void $ P.char '#' *> tillEndOfLine *> P.char '\n'

skippedLine :: Parser ()
skippedLine = void (P.many1 P.space) <|> comment

lineWords :: Parser [String]
lineWords = many (many horizSpace *> word <* many horizSpace)

cmdLine :: Parser String
cmdLine = do
  res <-
    (P.char '\t' *> tillEndOfLine <* P.char '\n') <|>
    ("" <$ skippedLine)
  return res

target :: Parser Target
target = do
  P.skipMany skippedLine
  outputPaths <- lineWords
  _ <- P.char ':'
  inputPaths <- lineWords
  _ <- P.char '\n'
  cmdLines <- filter (not . null) <$> many (P.try cmdLine)
  return Target
    { targetOutputPaths = outputPaths
    , targetInputHints = inputPaths
    , targetCmds = cmdLines
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

makefileParser :: Parser Makefile
makefileParser =
  (mkMakefile <$> many target) <* P.many skippedLine <* P.eof
