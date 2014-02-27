module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
import Control.Monad
import Data.List (partition)
import Data.Maybe (fromMaybe, listToMaybe)
import System.FilePath (takeDirectory, takeFileName)
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

parseLiteralString :: Parser String
parseLiteralString = do
  x <- P.char '\''
  str <- concat <$> many p
  y <- P.char '\''
  return $ concat [[x], str, [y]]
  where
    p = escapeSequence <|>
        ((: []) <$> P.satisfy (`notElem` "\n'"))

escapeSequence :: Parser String
escapeSequence = do
  esc <- P.char '\\'
  code <- P.anyChar
  return [esc, code]

variable :: [FilePath] -> [FilePath] -> Parser String
variable outputPaths inputPaths = do
  _ <- P.char '$'
  varId <- P.anyChar
  case varId of
    '(' -> do
      (multiVarId, multiType) <- (,) <$> P.anyChar <*> P.anyChar
      _ <- P.char ')'
      selector <-
        case multiType of
        'D' -> return takeDirectory
        'F' -> return takeFileName
        _ -> fail $ concat
             [ "Invalid selector "
             , show multiType, " in sequence: $("
             , [multiVarId, multiType], ")"
             ]
      varSelect multiVarId selector
    _ -> varSelect varId id
  where
    getFirst err paths = fromMaybe err $ listToMaybe paths
    varSelect varId toString =
      case varId of
      '@' -> return $ toString $ getFirst "No first output for @ variable" outputPaths
      '<' -> return $ toString $ getFirst "No first input for < variable" inputPaths
      '^' -> return $ unwords $ map toString inputPaths
      '|' -> error "TODO: order-only inputs"
      _ -> fail $ "Invalid variable id: " ++ [varId]

parseCmdChar :: [FilePath] -> [FilePath] -> Parser String
parseCmdChar outputPaths inputPaths =
  escapeSequence <|>
  variable outputPaths inputPaths <|>
  (: []) <$> P.satisfy (/= '\n')

interpolateCmdLine :: [FilePath] -> [FilePath] -> Parser String
interpolateCmdLine outputPaths inputPaths = concat <$> many (parseLiteralString <|> parseCmdChar outputPaths inputPaths)

cmdLine :: [FilePath] -> [FilePath] -> Parser String
cmdLine outputPaths inputPaths = do
  res <-
    (P.char '\t' *> interpolateCmdLine outputPaths inputPaths <* P.char '\n') <|>
    ("" <$ skippedLine)
  return res

target :: Parser Target
target = do
  P.skipMany skippedLine
  outputPaths <- lineWords
  _ <- P.char ':'
  inputPaths <- lineWords
  _ <- P.char '\n'
  cmdLines <- filter (not . null) <$> many (cmdLine outputPaths inputPaths)
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
