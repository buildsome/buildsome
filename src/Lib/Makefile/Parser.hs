{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveDataTypeable #-}
module Lib.Makefile.Parser
  ( makefile, parse, interpolateCmds, metaVariable
  , Vars, VarName, VarValue
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.List (partition)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Typeable (Typeable)
import Lib.ByteString (unprefixed)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile.CondState (CondState)
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
import qualified Lib.Makefile.CondState as CondState
import qualified Lib.StringPattern as StringPattern
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos

type VarName = ByteString
type VarValue = ByteString
type IncludeStack = [(Pos.SourcePos, ByteString)]

data Writer = Writer
  { writerTargets :: [Target]
  , writerPatterns :: [Pattern]
  }
instance Monoid Writer where
  mempty = Writer mempty mempty
  mappend (Writer ax ay) (Writer bx by) =
    Writer (mappend ax bx) (mappend ay by)

data State = State
  { stateIncludeStack :: IncludeStack
  , stateLocalsStack :: [Vars]
  , stateRootDir :: FilePath
  , stateVars :: Vars
  , stateCond :: CondState
  , stateWriter :: Writer -- would have used WriterT, but Parsec isn't easily liftable
  }
type Vars = Map VarName VarValue
type Parser = P.ParsecT ByteString State IO
type ParserG = P.ParsecT ByteString

atStateVars :: (Vars -> Vars) -> State -> State
atStateVars f state = state { stateVars = f (stateVars state) }

atStateIncludeStack :: (IncludeStack -> IncludeStack) -> State -> State
atStateIncludeStack f state = state { stateIncludeStack = f (stateIncludeStack state) }

atStateWriter :: (Writer -> Writer) -> State -> State
atStateWriter f state = state { stateWriter = f (stateWriter state) }

tell :: Writer -> Parser ()
tell w = P.updateState $ atStateWriter $ mappend w

tellTarget :: Target -> Parser ()
tellTarget x = tell mempty { writerTargets = [x] }

tellPattern :: Pattern -> Parser ()
tellPattern x = tell mempty { writerPatterns = [x] }

updateConds ::
  (CondState -> Either String CondState) -> Parser ()
updateConds f = do
  state <- P.getState
  case f $ stateCond state of
    Left err -> fail err
    Right r -> P.putState $ state { stateCond = r }

isCondTrue :: Parser Bool
isCondTrue = CondState.isTrue . stateCond <$> P.getState

whenCondTrue :: Parser () -> Parser ()
whenCondTrue act = do
  t <- isCondTrue
  when t act

-----------

horizSpace :: Monad m => ParserG u m Char
horizSpace = P.satisfy (`elem` " \t")

horizSpaces :: Monad m => ParserG u m ()
horizSpaces = P.skipMany horizSpace

horizSpaces1 :: Monad m => ParserG u m ()
horizSpaces1 = P.skipMany1 horizSpace

comment :: Monad m => ParserG u m ()
comment = void $ P.char '#' *> P.many (P.satisfy (/= '\n'))

newWord :: Monad m => ParserG u m ()
newWord = P.lookAhead $ void $ P.satisfy p
  where
    p x = not (isAlphaNum x || '_' == x)

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
    State includeStack localsStack rootDir vars cond writer <- P.getState
    case includeStack of
      [] -> fail "Don't steal eof"
      ((pos, input) : rest) -> do
        void $ P.setParserState P.State
          { P.statePos = pos
          , P.stateInput = input
          , P.stateUser = State rest localsStack rootDir vars cond writer
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

targetPattern :: Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser ()
targetPattern pos outputPaths inputPaths orderOnlyInputs = do
  -- Meta-variable interpolation must happen later, so allow $ to
  -- remain $ if variable fails to parse it
  cmdLines <- BS8.intercalate "\n" <$> P.many cmdLine
  whenCondTrue $ tellPattern $ Target
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

targetSimple :: Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser ()
targetSimple pos outputPaths inputPaths orderOnlyInputs = do
  cmdLines <- P.many cmdLine
  -- Immediately interpolate cmdLine metaVars (after having expanded ordinary vars):
  whenCondTrue $ tellTarget $ interpolateCmds Nothing Target
    { targetOutputs = outputPaths
    , targetInputs = inputPaths
    , targetOrderOnlyInputs = orderOnlyInputs
    , targetCmds = BS8.intercalate "\n" cmdLines
    , targetPos = pos
    }

-- Parses the target's entire lines (excluding the pre/post newlines)
target :: Parser ()
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
  let targetParser
        | "%" `BS8.isInfixOf` (BS8.concat . concat) [outputPaths, inputPaths, orderOnlyInputs] =
          targetPattern
        | otherwise = targetSimple
  targetParser pos outputPaths inputPaths orderOnlyInputs

data PosError = PosError Pos.SourcePos ByteString deriving (Typeable)
instance Show PosError where
  show (PosError pos msg) = concat [showPos pos, ": ", BS8.unpack msg]
instance E.Exception PosError

mkMakefile :: Writer -> Makefile
mkMakefile (Writer targets targetPatterns) =
  either E.throw id $ do
    phonies <- concat <$> mapM getPhonyInputs phonyTargets
    return Makefile
      { makefileTargets = regularTargets
      , makefilePatterns = targetPatterns
      , makefilePhonies = phonies
      }
  where
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

data OverrideVar = OverrideVar | DontOverrideVar

varAssignment :: Parser ()
varAssignment = do
  (varName, overrideVar) <-
      do x <- P.try $ ident
         horizSpaces
         y <-
           (OverrideVar     <$ P.string "=") <|>
           (DontOverrideVar <$ P.string "?=")
         horizSpaces
         return (x, y)
  value <- BS8.pack <$> P.many (unescapedChar <|> P.noneOf "#\n")
  skipLineSuffix
  let f DontOverrideVar (Just old) = Just old
      f _ _ = Just value
  whenCondTrue $ P.updateState $ atStateVars $
    M.alter (f overrideVar) varName

directive :: String -> Parser a -> Parser a
directive str act = do
  P.try $ P.optional (P.char ' ' *> horizSpaces) *> P.string str *> newWord
  horizSpaces *> act <* skipLineSuffix

echoDirective :: Parser ()
echoDirective = directive "echo" $ do
  str <- interpolateVariables unescapedSequence "#\n"
  whenCondTrue $ liftIO $ BS8.putStrLn $ "ECHO: " <> str

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
localDirective = directive "local" $ do
  P.choice
    [ P.char '{' *> localsOpen
    , P.char '}' *> localsClose
    ]

data IfType = IfEq | IfNeq

ifeqDirective :: String -> IfType -> Parser ()
ifeqDirective keyword ifType = directive keyword $ do
  _ <- P.char '('
  l <- readExp ','
  r <- readExp ')'
  updateConds $ return . CondState.nest (l `cmp` r)
  where
    readExp stopChar =
      horizSpaces *>
      interpolateVariables unescapedSequence [stopChar] <*
      horizSpaces <* P.char stopChar
    cmp = case ifType of
      IfEq -> (==)
      IfNeq -> (/=)

endifDirective :: Parser ()
endifDirective =
  directive "endif" $ updateConds $ CondState.unnest "endif mismatched"

elseDirective :: Parser ()
elseDirective =
  directive "else" $ updateConds (CondState.inverse "else mismatched")

conditionalStatement :: Parser ()
conditionalStatement = P.choice
  [ ifeqDirective "ifeq" IfEq
  , ifeqDirective "ifneq" IfNeq
  , elseDirective
  , endifDirective
  ]

makefile :: Parser Makefile
makefile =
  mkMakefile <$> do
    beginningOfLine -- due to beginning of file
    noiseLines
    -- TODO: Unnecessary buildup of empty tuple list here
    _ <- P.choice
      [ properEof
      , echoDirective
      , conditionalStatement
      , target
      , localDirective
      , varAssignment
      ] `P.sepBy` sepLines
    P.optional sepLines
    properEof
    stateWriter <$> P.getState
  where
    sepLines = newline *> noiseLines

parse :: FilePath -> Vars -> IO Makefile
parse makefileName vars = do
  res <-
    join $ parseFromFile makefile State
    { stateIncludeStack = []
    , stateLocalsStack = []
    , stateRootDir = FilePath.takeDirectory makefileName
    , stateVars = vars
    , stateCond = CondState.empty
    , stateWriter = mempty
    } makefileName
  case res of
    Right x -> return x
    Left err -> do
      hPutStrLn stderr $ showErr err
      fail "Makefile parse failure"
