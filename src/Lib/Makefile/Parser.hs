{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveDataTypeable #-}
module Lib.Makefile.Parser
  ( makefile, parse, interpolateCmds, metaVariable
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable (Typeable)
import Lib.ByteString (unprefixed)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile.Types
import Lib.Parsec (parseFromFile, showErr, showPos)
import Prelude hiding (FilePath)
import System.IO (hPutStrLn, stderr)
import Text.Parsec ((<?>))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Lib.FilePath as FilePath
import qualified Lib.StringPattern as StringPattern
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos

type VarName = ByteString
type VarValue = ByteString
type IncludeStack = [(Pos.SourcePos, ByteString)]
data State = State
  { stateIncludeStack :: IncludeStack
  , stateLocalsStack :: [Vars]
  , stateRootDir :: FilePath
  , stateVars :: Vars
  }
type Vars = Map VarName VarValue
type Parser = P.ParsecT ByteString State IO
type ParserG = P.ParsecT ByteString

atStateVars :: (Vars -> Vars) -> State -> State
atStateVars f (State i l r v) = State i l r (f v)

atStateIncludeStack :: (IncludeStack -> IncludeStack) -> State -> State
atStateIncludeStack f (State i l r v) = State (f i) l r v

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
filepaths = BS8.words <$> interpolateVariables unescapedSequence ":#|\n"

filepaths1 :: Parser [FilePath]
filepaths1 = do
  paths <- BS8.words <$> interpolateVariables unescapedSequence ":#|\n"
  if null paths
    then fail "need at least 1 file path"
    else return paths

escapeSequence :: Monad m => ParserG u m ByteString
escapeSequence = build <$> P.char '\\' <*> P.anyChar
  where
    build x y = BS8.singleton x <> BS8.singleton y

unescapedChar :: Monad m => ParserG u m Char
unescapedChar = P.char '\\' *> (unescape <$> P.anyChar)
  where
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 't' = '\t'
    unescape '\n' = ' '
    unescape x = x

unescapedSequence :: Monad m => ParserG u m ByteString
unescapedSequence = BS8.singleton <$> unescapedChar

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || x `elem` "_.~"

ident :: Monad m => ParserG u m ByteString
ident = BS8.pack <$> P.many1 (P.satisfy isIdentChar)

metaVarId ::
  Monad m => [FilePath] -> [FilePath] -> [FilePath] ->
  Maybe FilePath ->
  ParserG u m ((FilePath -> FilePath) -> FilePath)
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
    allInputs   toString = BS8.unwords $ map toString inputPaths
    allOOInputs toString = BS8.unwords $ map toString ooInputPaths

metaVarModifier :: Monad m => ParserG u m (FilePath -> FilePath)
metaVarModifier =
  P.choice
  [ FilePath.takeDirectory <$ P.char 'D'
  , FilePath.takeFileName  <$ P.char 'F'
  ]

metaVariable ::
  Monad m => [FilePath] -> [FilePath] -> [FilePath] ->
  Maybe ByteString -> ParserG u m ByteString
metaVariable outputPaths inputPaths ooInputPaths mStem =
  P.choice
  [ P.char '(' *> (vid <*> metaVarModifier) <* P.char ')'
  , vid <*> pure id
  ]
  where
    vid = metaVarId outputPaths inputPaths ooInputPaths mStem

-- Parse succeeds only if meta-variable, but preserve the meta-variable as is
preserveMetavar :: Monad m => ParserG u m ByteString
preserveMetavar =
  fmap ("$" <>) $
  (char4 <$> P.char '(' <*> P.oneOf "@<^|*" <*> P.oneOf "DF" <*> P.char ')') <|>
  (BS8.singleton <$> P.oneOf "@<^|*")
  where
    char4 a b c d = BS8.pack [a, b, c, d]

interpolateVariables :: (forall u m. Monad m => ParserG u m ByteString) -> String -> Parser ByteString
interpolateVariables escapeParse stopChars = do
  varsEnv <- stateVars <$> P.getState
  let
    interpolate :: Monad m => Set ByteString -> ParserG u m ByteString
    interpolate visitedVarNames = interpolateString escapeParse stopChars (variable visitedVarNames <|> preserveMetavar)
    variable :: Monad m => Set ByteString -> ParserG u m ByteString
    variable visitedVarNames = do
      -- '$' already parsed
      varName <- P.choice
        [ P.char '{' *> ident <* P.char '}'
        , BS8.singleton <$> P.satisfy isIdentChar
        ]
      pos <- P.getPosition
      when (varName `S.member` visitedVarNames) $
        error $ showPos pos ++ ": Recursive reference of variable to itself: " ++ show varName
      case M.lookup varName varsEnv of
        Nothing -> error $ showPos pos ++ ": No such variable: " ++ show varName
        Just val ->
          either (fail . show) return $
          P.runParser (P.setPosition pos *> interpolate (S.insert varName visitedVarNames) <* P.eof) () "" val
  interpolate S.empty

-- Inside a single line
interpolateString :: Monad m => ParserG u m ByteString -> String -> ParserG u m ByteString -> ParserG u m ByteString
interpolateString escapeParser stopChars dollarHandler =
  concatMany (literalString '\'' <|> doubleQuotes <|> interpolatedChar stopChars)
  where
    concatMany x = BS8.concat <$> P.many x
    doubleQuotes = doubleQuoted <$> P.char '"' <*> concatMany (interpolatedChar "\"\n") <*> P.char '"'
    doubleQuoted begin chars end = BS8.singleton begin <> chars <> BS8.singleton end
    interpolatedChar stopChars' = P.choice
      [ P.char '$' *> dollarHandler
      , escapeParser
      , BS8.singleton <$> P.noneOf stopChars'
      ]
    literalString delimiter = do
      x <- P.char delimiter
      str <- BS8.concat <$> P.many p
      y <- P.char delimiter
      return $ BS8.singleton x <> str <> BS8.singleton y
      where
        p = escapeSequence <|> (BS8.singleton <$> P.satisfy (`notElem` ['\n', delimiter]))

type IncludePath = FilePath

includeLine :: Parser IncludePath
includeLine = do
  fileNameStr <-
    P.try (horizSpaces *> P.string "include" *> horizSpace) *>
    horizSpaces *> interpolateVariables unescapedSequence " #\n" <* skipLineSuffix
  case reads (BS8.unpack fileNameStr) of
    [(path, "")] -> return path
    _ -> return fileNameStr

runInclude :: IncludePath -> Parser ()
runInclude rawIncludedPath = do
  includedPath <- computeIncludePath
  eFileContent <- liftIO $ E.try $ BS8.readFile $ BS8.unpack includedPath
  case eFileContent of
    Left e@E.SomeException {} -> fail $ "Failed to read include file: " ++ show e
    Right fileContent ->
      void $ P.updateParserState $ \(P.State input pos state) ->
        P.State fileContent (Pos.initialPos (BS8.unpack includedPath)) $
        atStateIncludeStack ((pos, input) :) state
  where
    computeIncludePath =
      case unprefixed "/" rawIncludedPath of
      Nothing -> do
        curPath <- FilePath.takeDirectory . BS8.pack . P.sourceName <$> P.getPosition
        return $ curPath </> rawIncludedPath
      Just pathSuffix -> do
        state <- P.getState
        return $ stateRootDir state </> pathSuffix

returnToIncluder :: Parser ()
returnToIncluder =
  P.eof *> do
    State includeStack localsStack rootDir vars <- P.getState
    case includeStack of
      [] -> fail "Don't steal eof"
      ((pos, input) : rest) -> do
        void $ P.setParserState P.State
          { P.statePos = pos
          , P.stateInput = input
          , P.stateUser = State rest localsStack rootDir vars
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

cmdLine :: Parser ByteString
cmdLine =
  ( P.try (newline *> noiseLines *> P.char '\t') *>
    interpolateVariables escapeSequence "#\n" <* skipLineSuffix
  ) <?> "cmd line"

mkFilePattern :: FilePath -> Maybe FilePattern
mkFilePattern path
  | "%" `BS8.isInfixOf` dir =
    error $ "Directory component may not be a pattern: " ++ show path
  | otherwise = FilePattern dir <$> StringPattern.fromString '%' file
  where
    (dir, file) = FilePath.splitFileName path

targetPattern :: Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser Pattern
targetPattern pos outputPaths inputPaths orderOnlyInputs = do
  -- Meta-variable interpolation must happen later, so allow $ to
  -- remain $ if variable fails to parse it
  cmdLines <- BS8.intercalate "\n" <$> P.many cmdLine
  return Target
    { targetOutputs = map mkOutputPattern outputPaths
    , targetInputs = inputPats
    , targetOrderOnlyInputs = orderOnlyInputPats
    , targetCmds = cmdLines
    , targetPos = pos
    }
  where
    mkOutputPattern outputPath =
      fromMaybe (error ("Outputs must all be patterns (contain %) in pattern rules: " ++ show outputPath)) $
      mkFilePattern outputPath
    inputPats = map tryMakePattern inputPaths
    orderOnlyInputPats = map tryMakePattern orderOnlyInputs
    tryMakePattern path = maybe (InputPath path) InputPattern $ mkFilePattern path

interpolateCmds :: Maybe ByteString -> Target -> Target
interpolateCmds mStem tgt@(Target outputs inputs ooInputs cmds pos) =
  tgt
  { targetCmds = either (error . show) id $ interpolateMetavars cmds
  }
  where
    interpolateMetavars =
      P.runParser
      ( P.setPosition pos
        *> (BS8.intercalate "\n" <$> (cmdInterpolate `P.sepBy` P.char '\n')) <*
        P.eof
      ) () ""
    cmdInterpolate =
      interpolateString escapeSequence "#\n"
      (metaVariable outputs inputs ooInputs mStem)
      <* skipLineSuffix

targetSimple :: Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser Target
targetSimple pos outputPaths inputPaths orderOnlyInputs = do
  cmdLines <- P.many cmdLine
  -- Immediately interpolate cmdLine metaVars (after having expanded ordinary vars):
  return $ interpolateCmds Nothing Target
    { targetOutputs = outputPaths
    , targetInputs = inputPaths
    , targetOrderOnlyInputs = orderOnlyInputs
    , targetCmds = BS8.intercalate "\n" cmdLines
    , targetPos = pos
    }

-- Parses the target's entire lines (excluding the pre/post newlines)
target :: Parser (Either Pattern Target)
target = do
  pos <- P.getPosition
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
  if "%" `BS8.isInfixOf` (BS8.concat . concat) [outputPaths, inputPaths, orderOnlyInputs]
    then Left <$> targetPattern pos outputPaths inputPaths orderOnlyInputs
    else Right <$> targetSimple pos outputPaths inputPaths orderOnlyInputs

data PosError = PosError Pos.SourcePos ByteString deriving (Typeable)
instance Show PosError where
  show (PosError pos msg) = concat [showPos pos, ": ", BS8.unpack msg]
instance E.Exception PosError

mkMakefile :: [Either Pattern Target] -> Makefile
mkMakefile allTargets =
  either E.throw id $ do
    phonies <- concat <$> mapM getPhonyInputs phonyTargets
    return Makefile
      { makefileTargets = regularTargets
      , makefilePatterns = targetPatterns
      , makefilePhonies = phonies
      }
  where
    (targetPatterns, targets) = partitionEithers allTargets
    outputPathsSet = S.fromList (concatMap targetOutputs regularTargets)
    badPhony t str = Left $ PosError (targetPos t) $ ".PHONY target " <> str
    getPhonyInputs t@(Target [".PHONY"] inputs [] cmd _) =
      case filter (`S.notMember` outputPathsSet) inputs of
      (danglingInput:_) -> badPhony t $ "refers to inexistent target " <> danglingInput
      [] | not (BS8.null cmd) -> badPhony t "may not specify commands"
         | otherwise -> return inputs
    getPhonyInputs t = badPhony t "invalid"
    (phonyTargets, regularTargets) = partition ((".PHONY" `elem`) . targetOutputs) targets

properEof :: Parser ()
properEof = do
  P.eof
  state <- P.getState
  unless (null (stateIncludeStack state)) $
    fail "EOF but includers still pending"
  case stateLocalsStack state of
    [] -> return ()
    stack -> fail $ "EOF: unterminated locals: " ++ show stack

varAssignment :: Parser ()
varAssignment = do
  varName <- P.try $ ident <* P.char '='
  value <- BS8.pack <$> P.many (unescapedChar <|> P.noneOf "#\n")
  skipLineSuffix
  P.updateState $ atStateVars $ M.insert varName value

echoStatement :: Parser ()
echoStatement = do
  P.try $ P.optional (P.char ' ' *> horizSpaces) *> P.string "echo" *> horizSpaces1
  str <- interpolateVariables unescapedSequence "#\n" <* skipLineSuffix
  liftIO $ BS8.putStrLn $ "ECHO: " <> str

localsOpen :: Parser ()
localsOpen = void $ P.updateState $ \state -> state { stateLocalsStack = stateVars state : stateLocalsStack state }

localsClose :: Parser ()
localsClose = do
  state <- P.getState
  case stateLocalsStack state of
    [] -> fail "Close of locals without open"
    (oldVars:rest) ->
      P.putState $ state { stateLocalsStack = rest, stateVars = oldVars }

localDirective :: Parser ()
localDirective = P.try (P.string "local" *> horizSpaces1) *> ((P.char '{' *> localsOpen) <|> (P.char '}' *> localsClose))

makefile :: Parser Makefile
makefile =
  mkMakefile . concat <$>
  ( beginningOfLine *> -- due to beginning of file
    noiseLines *>
    ( P.choice
      [ [] <$ properEof
      , [] <$ echoStatement
      , ((: []) <$> target) <|>
        (   []  <$  localDirective)
      , [] <$ varAssignment
      ]
      `P.sepBy` (newline *> noiseLines)
    ) <*
    P.optional (newline *> noiseLines) <*
    properEof
  )

parse :: FilePath -> IO Makefile
parse makefileName = do
  res <-
    join $ parseFromFile makefile State
    { stateIncludeStack = []
    , stateLocalsStack = []
    , stateRootDir = FilePath.takeDirectory makefileName
    , stateVars = M.empty
    } makefileName
  case res of
    Right x -> return x
    Left err -> do
      hPutStrLn stderr $ showErr err
      fail "Makefile parse failure"
