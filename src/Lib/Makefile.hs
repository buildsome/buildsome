module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
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

lineWords :: Parser [String]
lineWords = many (many horizSpace *> word <* many horizSpace)

cmdLine :: Parser String
cmdLine =
  (P.char '\t' *> P.many (P.satisfy (/= '\n'))) <|>
  ("" <$ (many horizSpace *> P.char '\n'))

target :: Parser Target
target = do
  outputPaths <- lineWords
  _ <- P.char ':'
  inputPaths <- lineWords
  _ <- P.char '\n'
  cmdLines <- filter (not . null) <$> many cmdLine
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
  (mkMakefile <$> many target) <* P.eof
