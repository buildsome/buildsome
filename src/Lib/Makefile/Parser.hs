{-# LANGUAGE Rank2Types #-}
module Lib.Makefile.Parser
  ( makefile, parse, interpolateCmds, metaVariable
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.List (partition, isInfixOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib.FilePath (splitFileName, (</>))
import Lib.List (unprefixed)
import Lib.Makefile.Types
import Lib.Parsec (parseFromFile, showErr, showPos)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.Parsec ((<?>))
import qualified Control.Exception as E
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lib.StringPattern as StringPattern
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos

data State = State
  { _includeStack :: [(Pos.SourcePos, String)]
  , _rootDirectory :: FilePath
  }
type Vars = Map String String
type Parser = P.ParsecT String State (StateT Vars IO)
type ParserG = P.ParsecT String

horizSpace :: Monad m => ParserG u m Char
horizSpace = P.satisfy (`elem` " \t")

horizSpaces :: Monad m => ParserG u m ()
horizSpaces = P.skipMany horizSpace

horizSpaces1 :: Monad m => ParserG u m ()
horizSpaces1 = P.skipMany1 horizSpace

comment :: Monad m => ParserG u m ()
comment = void $ P.char '#' *> P.many (P.satisfy (/= '\n'))

skipLineSuffix :: Monad m => ParserG u m ()
skipLineSuffix = horizSpaces <* P.optional comment <* P.lookAhead (void (P.char '\n') <|> P.eof)

filepaths :: Parser [FilePath]
filepaths = words <$> interpolateVariables unescapedSequence ":#|\n"

filepaths1 :: Parser [FilePath]
filepaths1 = do
  paths <- words <$> interpolateVariables unescapedSequence ":#|\n"
  if null paths
    then fail "need at least 1 file path"
    else return paths

escapeSequence :: Monad m => ParserG u m String
escapeSequence = build <$> P.char '\\' <*> P.anyChar
  where
    build x y = [x, y]

unescapedChar :: Monad m => ParserG u m Char
unescapedChar = P.char '\\' *> (unescape <$> P.anyChar)
  where
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 't' = '\t'
    unescape '\n' = ' '
    unescape x = x

unescapedSequence :: Monad m => ParserG u m String
unescapedSequence = (:[]) <$> unescapedChar

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || x `elem` "_.~"

ident :: Monad m => ParserG u m String
ident = P.many1 (P.satisfy isIdentChar)

metaVarId ::
  Monad m => [FilePath] -> [FilePath] -> [FilePath] ->
  Maybe String -> ParserG u m ((String -> String) -> String)
metaVarId outputPaths inputPaths ooInputPaths mStem =
  P.choice $
  [ firstOutput <$ P.char '@'
  , firstInput  <$ P.char '<'
  , allInputs   <$ P.char '^'
  , allOOInputs <$ P.char '|'
  ] ++
  [ ($ stem) <$ P.char '*'
  | Just stem <- [mStem]
  ]
  where
    getFirst err paths = fromMaybe (error err) $ listToMaybe paths
    firstOutput toString = toString $ getFirst "No first output for @ variable" outputPaths
    firstInput  toString = toString $ getFirst "No first input for < variable"  inputPaths
    allInputs   toString = unwords $ map toString inputPaths
    allOOInputs toString = unwords $ map toString ooInputPaths

metaVarModifier :: Monad m => ParserG u m (String -> String)
metaVarModifier =
  P.choice
  [ takeDirectory <$ P.char 'D'
  , takeFileName  <$ P.char 'F'
  ]

metaVariable :: Monad m => [FilePath] -> [FilePath] -> [FilePath] -> Maybe String -> ParserG u m String
metaVariable outputPaths inputPaths ooInputPaths mStem =
  P.choice
  [ P.char '(' *> (vid <*> metaVarModifier) <* P.char ')'
  , vid <*> pure id
  ]
  where
    vid = metaVarId outputPaths inputPaths ooInputPaths mStem

-- Parse succeeds only if meta-variable, but preserve the meta-variable as is
preserveMetavar :: Monad m => ParserG u m String
preserveMetavar =
  fmap ('$':) $
  (char4 <$> P.char '(' <*> P.oneOf "@<^|*" <*> P.oneOf "DF" <*> P.char ')') <|>
  ((: []) <$> P.oneOf "@<^|*")
  where
    char4 a b c d = [a, b, c, d]

interpolateVariables :: (forall u m. Monad m => ParserG u m String) -> String -> Parser String
interpolateVariables escapeParse stopChars = do
  varsEnv <- lift State.get
  curDir <- takeDirectory . P.sourceName <$> P.getPosition
  let
    curDirVar :: Monad m => ParserG u m String
    curDirVar = curDir <$ P.char '.'
    interpolate :: Monad m => ParserG u m String
    interpolate = interpolateString escapeParse stopChars (curDirVar <|> variable <|> preserveMetavar)
    variable :: Monad m => ParserG u m String
    variable = do
      -- '$' already parsed
      varName <- P.choice
        [ P.char '{' *> ident <* P.char '}'
        , (:[]) <$> P.satisfy isIdentChar
        ]
      case M.lookup varName varsEnv of
        Nothing -> do
          pos <- P.getPosition
          error $ showPos pos ++ ": No such variable: " ++ show varName
        Just val ->
          either (fail . show) return $
          P.runParser interpolate () "" val
  interpolate

-- Inside a single line
interpolateString :: Monad m => ParserG u m String -> [Char] -> ParserG u m String -> ParserG u m String
interpolateString escapeParser stopChars dollarHandler =
  concatMany (literalString '\'' <|> doubleQuotes <|> interpolatedChar stopChars)
  where
    concatMany x = concat <$> P.many x
    doubleQuotes = doubleQuoted <$> P.char '"' <*> concatMany (interpolatedChar "\"\n") <*> P.char '"'
    doubleQuoted begin chars end = concat [[begin], chars, [end]]
    interpolatedChar stopChars' = P.choice
      [ (P.char '$' *> dollarHandler)
      , escapeParser
      , (: []) <$> P.noneOf stopChars'
      ]
    literalString delimiter = do
      x <- P.char delimiter
      str <- concat <$> P.many p
      y <- P.char delimiter
      return $ concat [[x], str, [y]]
      where
        p = escapeSequence <|> ((: []) <$> P.satisfy (`notElem` ['\n', delimiter]))

type IncludePath = FilePath

includeLine :: Parser IncludePath
includeLine = do
  fileNameStr <-
    P.try (horizSpaces *> P.string "include" *> horizSpace) *>
    horizSpaces *> interpolateVariables unescapedSequence " #\n" <* skipLineSuffix
  case reads fileNameStr of
    [(path, "")] -> return path
    _ -> return fileNameStr

runInclude :: IncludePath -> Parser ()
runInclude rawIncludedPath = do
  includedPath <- computeIncludePath
  eFileContent <- liftIO $ E.try $ readFile includedPath
  case eFileContent of
    Left e@E.SomeException {} -> fail $ "Failed to read include file: " ++ show e
    Right fileContent ->
      void $ P.updateParserState $ \(P.State input pos (State includeStack rootDir)) ->
        P.State fileContent (Pos.initialPos includedPath) $
        State ((pos, input) : includeStack) rootDir
  where
    computeIncludePath =
      case unprefixed "ROOT/" rawIncludedPath of
      Nothing -> do
        curPath <- takeDirectory . P.sourceName <$> P.getPosition
        return $ curPath </> rawIncludedPath
      Just pathSuffix -> do
        State _ rootDir <- P.getState
        return $ rootDir </> pathSuffix

returnToIncluder :: Parser ()
returnToIncluder =
  P.eof *> do
    State posStack rootDir <- P.getState
    case posStack of
      [] -> fail "Don't steal eof"
      ((pos, input) : rest) -> do
        void $ P.setParserState P.State
          { P.statePos = pos
          , P.stateInput = input
          , P.stateUser = State rest rootDir
          }
        -- "include" did not eat the end of line (if one existed) so lets
        -- read it here
        P.optional (P.char '\n') <?> "newline after include statement"

beginningOfLine :: Parser ()
beginningOfLine = do
  mIncludePath <- P.optionMaybe includeLine
  case mIncludePath of
    Just includePath -> runInclude includePath *> beginningOfLine
    Nothing -> -- A line that begins with eof can still lead to a new line in the includer
      P.optional $ returnToIncluder *> beginningOfLine

-- Called to indicate we are willing to start a new line, which can be
-- done in two ways:
-- * A simple newline char
-- * End of file that leads back to the includer.
--
-- Either way, we get to the beginning of a new line, and despite
-- Parsec unconvinced, we do make progress in both, so it is safe for
-- Applicative.many (P.many complains)
newline :: Parser ()
newline = (returnToIncluder <|> void (P.char '\n')) *> beginningOfLine

-- we're at the beginning of a line, and we can eat
-- whitespace-only lines, as long as we also eat all the way to
-- the end of line (including the next newline if it exists)
-- Always succeeds, but may eat nothing at all:
noiseLines :: Parser ()
noiseLines =
  P.try (horizSpaces1 *> ((eol *> noiseLines) <|> properEof)) <|>
  P.optional (P.try (eol *> noiseLines))
  where
    eol = skipLineSuffix *> newline

cmdLine :: Parser String
cmdLine =
  ( P.try (newline *> noiseLines *> P.char '\t') *>
    interpolateVariables escapeSequence "#\n" <* skipLineSuffix
  ) <?> "cmd line"

-- TODO: Better canonization
canonizeCmdLines :: [String] -> [String]
canonizeCmdLines = filter (not . null)

mkFilePattern :: FilePath -> Maybe FilePattern
mkFilePattern path
  | "%" `isInfixOf` dir =
    error $ "Directory component may not be a pattern: " ++ show path
  | otherwise = FilePattern dir <$> StringPattern.fromString "%" file
  where
    (dir, file) = splitFileName path

targetPattern :: [FilePath] -> [FilePath] -> [FilePath] -> Parser Pattern
targetPattern outputPaths inputPaths orderOnlyInputs = do
  -- Meta-variable interpolation must happen later, so allow $ to
  -- remain $ if variable fails to parse it
  cmdLines <- P.many cmdLine
  return Target
    { targetOutputs = map mkOutputPattern outputPaths
    , targetInputs = inputPats
    , targetOrderOnlyInputs = orderOnlyInputPats
    , targetCmds = canonizeCmdLines cmdLines
    }
  where
    mkOutputPattern outputPath =
      fromMaybe (error ("Outputs must all be patterns (contain %) in pattern rules: " ++ show outputPath)) $
      mkFilePattern outputPath
    inputPats = map tryMakePattern inputPaths
    orderOnlyInputPats = map tryMakePattern orderOnlyInputs
    tryMakePattern path = maybe (InputPath path) InputPattern $ mkFilePattern path

interpolateCmds :: Maybe String -> Target -> Target
interpolateCmds mStem tgt@(Target outputs inputs ooInputs cmds) =
  tgt
  { targetCmds = either (error . show) id $ mapM interpolateMetavars cmds
  }
  where
    interpolateMetavars = P.runParser cmdInterpolate () (show tgt)
    cmdInterpolate =
      interpolateString escapeSequence "#\n" $ metaVariable outputs inputs ooInputs mStem

targetSimple :: [FilePath] -> [FilePath] -> [FilePath] -> Parser Target
targetSimple outputPaths inputPaths orderOnlyInputs = do
  cmdLines <- P.many cmdLine
  -- Immediately interpolate cmdLine metaVars (after having expanded ordinary vars):
  return $ interpolateCmds Nothing Target
    { targetOutputs = outputPaths
    , targetInputs = inputPaths
    , targetOrderOnlyInputs = orderOnlyInputs
    , targetCmds = canonizeCmdLines cmdLines
    }

-- Parses the target's entire lines (excluding the pre/post newlines)
target :: Parser (Either Pattern Target)
target = do
  outputPaths <- P.try $
    -- disallow tab here
    P.skipMany (P.char ' ') *>
    (filepaths1 <?> "outputs") <*
    horizSpaces <* P.char ':'
  horizSpaces
  inputPaths <- filepaths <?> "inputs"
  orderOnlyInputs <-
    fmap (fromMaybe []) . P.optionMaybe $
    P.try (horizSpaces *> P.char '|') *> horizSpaces *> filepaths
  skipLineSuffix
  if "%" `isInfixOf` (concat . concat) [outputPaths, inputPaths, orderOnlyInputs]
    then Left <$> targetPattern outputPaths inputPaths orderOnlyInputs
    else Right <$> targetSimple outputPaths inputPaths orderOnlyInputs

mkMakefile :: [Either Pattern Target] -> Makefile
mkMakefile allTargets
  | not $ null $ concatMap targetCmds phonyTargets = error ".PHONY targets may not have commands!"
  | not $ null missingPhonies = error $ "missing .PHONY targets: " ++ show missingPhonies
  | otherwise =
    Makefile
    { makefileTargets = regularTargets
    , makefilePatterns = targetPatterns
    , makefilePhonies = phonies
    }
  where
    (targetPatterns, targets) = partitionEithers allTargets
    missingPhonies = S.toList $ S.fromList phonies `S.difference` outputPathsSet
    outputPathsSet = S.fromList (concatMap targetOutputs regularTargets)
    phonies = concatMap getPhonyInputs phonyTargets
    getPhonyInputs (Target [".PHONY"] inputs [] []) = inputs
    getPhonyInputs t = error $ "Invalid .PHONY target: " ++ show t
    (phonyTargets, regularTargets) = partition ((".PHONY" `elem`) . targetOutputs) targets

properEof :: Parser ()
properEof = do
  P.eof
  State posStack _ <- P.getState
  case posStack of
    [] -> return ()
    _ -> fail "EOF but includers still pending"

varAssignment :: Parser ()
varAssignment = do
  varName <- P.try $ ident <* P.char '='
  value <- P.many (unescapedChar <|> P.noneOf "#\n")
  skipLineSuffix
  lift $ State.modify (M.insert varName value)

echoStatement :: Parser ()
echoStatement = do
  P.try $ P.optional (P.char ' ' *> horizSpaces) *> P.string "echo" *> horizSpaces1
  str <- interpolateVariables unescapedSequence "#\n" <* skipLineSuffix
  liftIO $ putStrLn $ "ECHO: " ++ str

makefile :: Parser Makefile
makefile =
  mkMakefile . concat <$>
  ( beginningOfLine *> -- due to beginning of file
    noiseLines *>
    ( P.choice
      [ [] <$ properEof
      , [] <$ echoStatement
      , (: []) <$> target
      , [] <$ varAssignment
      ]
      `P.sepBy` (newline *> noiseLines)
    ) <*
    P.optional (newline *> noiseLines) <*
    properEof
  )

parse :: FilePath -> IO Makefile
parse makefileName = do
  parseAction <- parseFromFile makefile (State [] (takeDirectory makefileName)) makefileName
  res <- evalStateT parseAction M.empty
  case res of
    Right x -> return x
    Left err -> do
      hPutStrLn stderr $ showErr err
      fail "Makefile parse failure"
