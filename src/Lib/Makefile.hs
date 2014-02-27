module Lib.Makefile
  ( Target(..)
  , Makefile(..)
  , makefileParser
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import Data.Maybe (fromMaybe, listToMaybe)
import System.FilePath (takeDirectory, takeFileName)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos

data Target = Target
  { targetOutputPaths :: [FilePath]
  , targetExplicitInputHints :: [FilePath]
  , targetOrderOnlyInputHints :: [FilePath]
  , targetCmds :: [String]
  } deriving (Show)

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePhonies :: [FilePath]
  } deriving (Show)

type IncludeStack = [(Pos.SourcePos, String)]

type Parser = P.ParsecT String IncludeStack IO

filepath :: Parser FilePath
filepath = some $ P.satisfy $ \x -> x `notElem` ":#| \t\r\n"

horizSpace :: Parser Char
horizSpace = P.satisfy (`elem` " \t")

tillEndOfLine :: Parser String
tillEndOfLine = P.many (P.satisfy (/= '\n'))

comment :: Parser ()
comment = void $ P.char '#' *> tillEndOfLine

skipLineSuffix :: Parser ()
skipLineSuffix = P.skipMany horizSpace <* optional comment <* P.char '\n'

filepaths :: Parser [String]
filepaths = many (many horizSpace *> filepath <* many horizSpace)

literalString :: Char -> Parser String
literalString delimiter = do
  x <- P.char delimiter
  str <- concat <$> many p
  y <- P.char delimiter
  return $ concat [[x], str, [y]]
  where
    p = escapeSequence <|>
        ((: []) <$> P.satisfy (`notElem` ['\n', delimiter]))

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
        _ -> P.unexpected $ concat
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
      _ -> P.unexpected $ "Invalid variable id: " ++ [varId]

parseCmdChar :: [FilePath] -> [FilePath] -> Parser String
parseCmdChar outputPaths inputPaths =
  escapeSequence <|>
  variable outputPaths inputPaths <|>
  (: []) <$> P.satisfy (/= '\n')

interpolateCmdLine :: [FilePath] -> [FilePath] -> Parser String
interpolateCmdLine outputPaths inputPaths =
  concat <$> many (literalString '\'' <|> parseCmdChar outputPaths inputPaths)

includeDirective :: Parser FilePath
includeDirective = do
  fileNameStr <- P.string "include" *> many horizSpace *> (literalString '"' <|> (wrap <$> tillEndOfLine)) <* skipLineSuffix
  case reads fileNameStr of
    [(path, "")] -> return path
    _ -> P.unexpected $ "Malformed include statement: " ++ show fileNameStr
  where
    wrap x = concat ["\"", x, "\""]

include :: Parser ()
include = do
  includedPath <- includeDirective
  fileContent <- liftIO $ readFile includedPath
  void $ P.updateParserState $ \oldState -> oldState
    { P.stateInput = fileContent
    , P.statePos = Pos.initialPos includedPath
    , P.stateUser = (P.statePos oldState, P.stateInput oldState) : P.stateUser oldState
    }

popStack :: Parser ()
popStack =
  P.eof *> do
    posStack <- P.getState
    case posStack of
      [] -> fail "Don't steal eof"
      ((pos, input) : rest) ->
        void $ P.setParserState
        P.State { P.statePos = pos, P.stateInput = input, P.stateUser = rest }

commonLine :: Parser ()
commonLine = many horizSpace *> (include <|> skipLineSuffix <|> popStack)

cmdLine :: [FilePath] -> [FilePath] -> Parser String
cmdLine outputPaths inputPaths =
  (P.char '\t' *> interpolateCmdLine outputPaths inputPaths <* skipLineSuffix) <|>
  ("" <$ commonLine)

target :: Parser Target
target = do
  P.skipMany commonLine
  outputPaths <- filepaths
  _ <- P.char ':'
  inputPaths <- filepaths
  orderOnlyInputs <- optional $ P.char '|' *> filepaths
  _ <- skipLineSuffix
  cmdLines <- filter (not . null) <$> many (cmdLine outputPaths inputPaths)
  return Target
    { targetOutputPaths = outputPaths
    , targetExplicitInputHints = inputPaths
    , targetOrderOnlyInputHints = fromMaybe [] orderOnlyInputs
    , targetCmds = cmdLines
    }



mkMakefile :: [Target] -> Makefile
mkMakefile targets
  | not $ null $ concatMap targetCmds phonyTargets = error ".PHONY targets may not have commands!"
  | otherwise =
    Makefile
    { makefileTargets = regularTargets
    , makefilePhonies = concatMap getPhonyInputs phonyTargets
    }
  where
    getPhonyInputs (Target [".PHONY"] inputs [] []) = inputs
    getPhonyInputs t = error $ "Invalid .PHONY target: " ++ show t
    (phonyTargets, regularTargets) = partition ((".PHONY" `elem`) . targetOutputPaths) targets

makefileParser :: Parser Makefile
makefileParser =
  (mkMakefile <$> many target) <* P.skipMany commonLine <* P.eof
