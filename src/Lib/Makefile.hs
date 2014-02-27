module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
import Data.List (partition)
import qualified Data.Char as C
import qualified Text.Parser.Char as P
import qualified Text.Trifecta as T

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

word :: T.Parser String
word = some (P.satisfy (not . isSeparator))

horizSpace :: T.Parser Char
horizSpace = P.satisfy $ \x -> x == ' ' || x == '\t'

lineWords :: T.Parser [String]
lineWords = many (many horizSpace *> word <* many horizSpace)

cmdLine :: T.Parser String
cmdLine =
  (P.char '\t' *> T.many (P.satisfy (/= '\n'))) <|>
  ("" <$ (many horizSpace *> P.char '\n'))

target :: T.Parser Target
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

makefileParser :: T.Parser Makefile
makefileParser =
  (mkMakefile <$> many target) <* T.eof
