{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types, OverloadedStrings, DeriveDataTypeable, CPP, FlexibleContexts #-}
module Lib.Makefile.Parser
  ( makefile, parse, interpolateCmds
  , Vars, VarName, VarValue
  ) where

import           Control.Applicative ((<|>))
import qualified Control.Exception as E
import           Control.Monad (when, unless, join, void)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isAlphaNum)
import           Data.List (partition)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Lib.ByteString (unprefixed)
import           Lib.Cartesian (cartesian)
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import           Lib.Makefile.CondState (CondState)
import qualified Lib.Makefile.CondState as CondState
import           Lib.Makefile.MonadClass (MonadMakefileParser)
import qualified Lib.Makefile.MonadClass as MakefileMonad
import           Lib.Makefile.Types
import           Lib.Parsec (showErr, showPos)
import qualified Lib.StringPattern as StringPattern
import           Text.Parsec ((<?>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos
import qualified Text.Parsec.Prim as Prim

import qualified BMake.Interpreter as BMake
import           Prelude.Compat hiding (FilePath)

#define RELEASE_INLINE(x)   {-# INLINE x #-}
-- #define RELEASE_INLINE(x)

type IncludeStack = [(Pos.SourcePos, ByteString)]

data Writer = Writer
  { writerTargets :: [Target]
  , writerPatterns :: [Pattern]
  , writerWeakVars :: Map VarName ByteString
  }
instance Monoid Writer where
  mempty = Writer mempty mempty mempty
  mappend (Writer ax ay az) (Writer bx by bz) =
    Writer (mappend ax bx) (mappend ay by) (mappend az bz)

data State = State
  { stateIncludeStack :: IncludeStack
  , stateLocalsStack :: [Vars]
  , stateRootDir :: FilePath
  , stateVars :: Vars
  , stateCond :: CondState
  , stateWriter :: Writer -- would have used WriterT, but Parsec isn't easily liftable
  }
type Parser m = P.ParsecT ByteString State m
type ParserG = P.ParsecT ByteString

atStateVars :: (Vars -> Vars) -> State -> State
atStateVars f state = state { stateVars = f (stateVars state) }

atStateIncludeStack :: (IncludeStack -> IncludeStack) -> State -> State
atStateIncludeStack f state = state { stateIncludeStack = f (stateIncludeStack state) }

atStateWriter :: (Writer -> Writer) -> State -> State
atStateWriter f state = state { stateWriter = f (stateWriter state) }

RELEASE_INLINE(tell)
tell :: Monad m => Writer -> Parser m ()
tell w = P.updateState $ atStateWriter $ mappend w

RELEASE_INLINE(tellTarget)
tellTarget :: Monad m => Target -> Parser m ()
tellTarget x = tell mempty { writerTargets = [x] }

RELEASE_INLINE(tellPattern)
tellPattern :: Monad m => Pattern -> Parser m ()
tellPattern x = tell mempty { writerPatterns = [x] }

RELEASE_INLINE(tellWeakVar)
tellWeakVar :: Monad m => VarName -> ByteString -> Parser m ()
tellWeakVar varName defaultVal = tell mempty { writerWeakVars = M.singleton varName defaultVal }

RELEASE_INLINE(updateConds)
updateConds :: Monad m => (CondState -> Either String CondState) -> Parser m ()
updateConds f = do
  state <- P.getState
  case f $ stateCond state of
    Left err -> fail err
    Right r -> P.putState $ state { stateCond = r }

RELEASE_INLINE(isCondTrue)
isCondTrue :: Monad m => Parser m Bool
isCondTrue = CondState.isTrue . stateCond <$> P.getState

RELEASE_INLINE(whenCondTrue)
whenCondTrue :: Monad m => Parser m () -> Parser m ()
whenCondTrue act = do
  t <- isCondTrue
  when t act

-----------

RELEASE_INLINE(horizSpace)
horizSpace :: Monad m => ParserG u m Char
horizSpace = P.char ' '

RELEASE_INLINE(horizSpaces)
horizSpaces :: Monad m => ParserG u m ()
horizSpaces = P.skipMany horizSpace

RELEASE_INLINE(horizSpaces1)
horizSpaces1 :: Monad m => ParserG u m ()
horizSpaces1 = P.skipMany1 horizSpace

RELEASE_INLINE(comment)
comment :: Monad m => ParserG u m ()
comment = void $ P.char '#' *> P.many (P.satisfy (`BS8.notElem` "\n"))

RELEASE_INLINE(newWord)
newWord :: Monad m => ParserG u m ()
newWord = P.lookAhead $ void $ P.satisfy p
  where
    p x = not (isAlphaNum x || '_' == x)

RELEASE_INLINE(skipLineSuffix)
skipLineSuffix :: Monad m => ParserG u m ()
skipLineSuffix = horizSpaces <* P.optional comment <* P.lookAhead (void (P.char '\n') <|> P.eof)

RELEASE_INLINE(filepaths)
filepaths :: Monad m => Parser m [FilePath]
filepaths = do
  pos <- P.getPosition
  BS8.words . cartesian pos <$> interpolateVariables unescapedSequence ":#|\n"

RELEASE_INLINE(filepaths1)
filepaths1 :: Monad m => Parser m [FilePath]
filepaths1 = do
  paths <- filepaths
  if null paths
    then fail "need at least 1 file path"
    else return paths

RELEASE_INLINE(escapeSequence)
escapeSequence :: Monad m => ParserG u m ByteString
escapeSequence = build <$> P.char '\\' <*> P.anyChar
  where
    build x y = BS8.singleton x <> BS8.singleton y

RELEASE_INLINE(unescapedChar)
unescapedChar :: Monad m => ParserG u m Char
unescapedChar = P.char '\\' *> (unescape <$> P.anyChar)
  where
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 't' = '\t'
    unescape '\n' = ' '
    unescape x = x

RELEASE_INLINE(unescapedSequence)
unescapedSequence :: Monad m => ParserG u m ByteString
unescapedSequence = BS8.singleton <$> unescapedChar

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || x `BS8.elem` "_.~"

RELEASE_INLINE(ident)
ident :: Monad m => ParserG u m ByteString
ident = BS8.pack <$> P.many1 (P.satisfy isIdentChar)

RELEASE_INLINE(metaVarId)
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

RELEASE_INLINE(metaVarModifier)
metaVarModifier :: Monad m => ParserG u m (FilePath -> FilePath)
metaVarModifier =
  P.choice
  [ FilePath.takeDirectory <$ P.char 'D'
  , FilePath.takeFileName  <$ P.char 'F'
  ]

RELEASE_INLINE(metaVariable)
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
RELEASE_INLINE(preserveMetavar)
preserveMetavar :: Monad m => ParserG u m ByteString
preserveMetavar =
  fmap ("$" <>) $
  (char4 <$> P.char '(' <*> P.oneOf "@<^|*" <*> P.oneOf "DF" <*> P.char ')') <|>
  (BS8.singleton <$> P.oneOf "@<^|*")
  where
    char4 a b c d = BS8.pack [a, b, c, d]

RELEASE_INLINE(interpolateVariables)
interpolateVariables :: Monad n => (forall u m. Monad m => ParserG u m ByteString) -> String -> Parser n ByteString
interpolateVariables escapeParse stopChars = do
  varsEnv <- stateVars <$> P.getState
  let
    interpolate :: Monad m => Set ByteString -> ParserG u m ByteString
    interpolate visitedVarNames = interpolateString escapeParse stopChars (variable visitedVarNames <|> preserveMetavar)
    RELEASE_INLINE(variable)
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
RELEASE_INLINE(interpolateString)
interpolateString ::
  Monad m => ParserG u m ByteString -> String -> ParserG u m ByteString -> ParserG u m ByteString
interpolateString escapeParser stopChars dollarHandler =
  concatMany (interpolatedChar ('\'':'"':stopChars) <|> literalString '\'' <|> doubleQuotes)
  where
    concatMany x = BS8.concat <$> P.many x
    RELEASE_INLINE(doubleQuotes)
    doubleQuotes = doubleQuoted <$> P.char '"' <*> concatMany (interpolatedChar "\"\n") <*> P.char '"'
    doubleQuoted begin chars end = BS8.singleton begin <> chars <> BS8.singleton end
    RELEASE_INLINE(interpolatedChar)
    interpolatedChar stopChars' = P.choice
      [ BS8.singleton <$> P.noneOf ('\t' : '\\' : '$' :stopChars')
      , P.char '$' *> dollarHandler
      , escapeParser
      ]
    RELEASE_INLINE(literalString)
    literalString delimiter = do
      x <- P.char delimiter
      str <- BS8.concat <$> P.many p
      y <- P.char delimiter
      return $ BS8.singleton x <> str <> BS8.singleton y
      where
        p = escapeSequence <|> (BS8.singleton <$> P.satisfy (`notElem` ['\t', '\n', delimiter]))

type IncludePath = FilePath

RELEASE_INLINE(includeLine)
includeLine :: Monad m => Parser m IncludePath
includeLine = do
  fileNameStr <-
    P.try (horizSpaces *> P.string "include" *> horizSpace) *>
    horizSpaces *> interpolateVariables unescapedSequence " #\n" <* skipLineSuffix
  case reads (BS8.unpack fileNameStr) of
    [(path, "")] -> return path
    _ -> return fileNameStr

RELEASE_INLINE(runInclude)
runInclude :: MonadMakefileParser m => IncludePath -> Parser m ()
runInclude rawIncludedPath = do
  includedPath <- computeIncludePath
  eFileContent <- lift $ MakefileMonad.tryReadFile includedPath
  case eFileContent of
    Left e@E.SomeException {} -> fail $ "Failed to read include file: " ++ show e
    Right fileContent ->
      void $ P.updateParserState $ \(Prim.State input pos state) ->
        Prim.State fileContent (Pos.initialPos (BS8.unpack includedPath)) $
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

RELEASE_INLINE(returnToIncluder)
returnToIncluder :: Monad m => Parser m ()
returnToIncluder =
  P.eof *> do
    State includeStack localsStack rootDir vars cond writer <- P.getState
    case includeStack of
      [] -> fail "Don't steal eof"
      ((pos, input) : rest) -> do
        void $ P.setParserState Prim.State
          { Prim.statePos = pos
          , Prim.stateInput = input
          , Prim.stateUser = State rest localsStack rootDir vars cond writer
          }
        -- "include" did not eat the end of line (if one existed) so lets
        -- read it here
        P.optional (P.char '\n') <?> "newline after include statement"

RELEASE_INLINE(beginningOfLine)
beginningOfLine :: MonadMakefileParser m => Parser m ()
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
RELEASE_INLINE(newline)
newline :: MonadMakefileParser m => Parser m ()
newline = (returnToIncluder <|> void (P.char '\n')) *> beginningOfLine

-- we're at the beginning of a line, and we can eat
-- whitespace-only lines, as long as we also eat all the way to
-- the end of line (including the next newline if it exists)
-- Always succeeds, but may eat nothing at all:
RELEASE_INLINE(noiseLines)
noiseLines :: MonadMakefileParser m => Parser m ()
noiseLines =
  P.try (horizSpaces1 *> ((eol *> noiseLines) <|> properEof)) <|>
  P.optional (P.try (eol *> noiseLines))
  where
    eol = skipLineSuffix *> newline

RELEASE_INLINE(cmdLine)
cmdLine :: MonadMakefileParser m => Parser m ByteString
cmdLine =
  ( P.try (newline *> noiseLines *> P.char '\t')
    *> interpolateVariables escapeSequence "#\n"
    <* skipLineSuffix
    ) <?> "cmd line"

mkFilePattern :: FilePath -> Maybe FilePattern
mkFilePattern path
  | "%" `BS8.isInfixOf` dir =
    error $ "Directory component may not be a pattern: " ++ show path
  | otherwise = FilePattern dir <$> StringPattern.fromString file
  where
    (dir, file) = FilePath.splitFileName path

RELEASE_INLINE(targetPattern)
targetPattern :: MonadMakefileParser m => Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser m ()
targetPattern pos outputPaths inputPaths orderOnlyInputs = do
  -- Meta-variable interpolation must happen later, so allow $ to
  -- remain $ if variable fails to parse it
  cmdLines <- BS8.intercalate "\n" <$> P.many cmdLine
  whenCondTrue $ tellPattern Target
    { targetOutputs = map mkOutputPattern outputPaths
    , targetInputs = inputPats
    , targetOrderOnlyInputs = orderOnlyInputPats
    , targetCmds = Left cmdLines
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
interpolateCmds mStem@_ tgt@(Target _ _ _ (Right _) _) = BMake.interpolateCmds mStem tgt
interpolateCmds mStem tgt@(Target outputs inputs ooInputs (Left cmds) pos) =
  tgt
  { targetCmds = Left $ either (error . show) id $ interpolateMetavars cmds
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

RELEASE_INLINE(targetSimple)
targetSimple :: MonadMakefileParser m => Pos.SourcePos -> [FilePath] -> [FilePath] -> [FilePath] -> Parser m ()
targetSimple pos outputPaths inputPaths orderOnlyInputs = do
  cmdLines <- P.many cmdLine
  -- Immediately interpolate cmdLine metaVars (after having expanded ordinary vars):
  whenCondTrue $ tellTarget $ interpolateCmds Nothing Target
    { targetOutputs = outputPaths
    , targetInputs = inputPaths
    , targetOrderOnlyInputs = orderOnlyInputs
    , targetCmds = Left $ BS8.intercalate "\n" cmdLines
    , targetPos = pos
    }

-- Parses the target's entire lines (excluding the pre/post newlines)
RELEASE_INLINE(target)
target :: MonadMakefileParser m => Parser m ()
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
instance E.Exception PosError
instance Show PosError where
  show (PosError pos msg) = concat [showPos pos, ": ", BS8.unpack msg]

mkMakefile :: Writer -> Makefile
mkMakefile (Writer targets targetPatterns weakVars) =
  either E.throw id $ do
    phonies <- concat <$> mapM getPhonyInputs phonyTargets
    return Makefile
      { makefileTargets = regularTargets
      , makefilePatterns = targetPatterns
      , makefilePhonies = phonies
      , makefileWeakVars = weakVars
      }
  where
    badPhony t str = Left $ PosError (targetPos t) $ ".PHONY target " <> str
    getPhonyInputs t@(Target [".PHONY"] inputs [] (Left cmd) _)
      | not (BS8.null cmd) = badPhony t "may not specify commands"
      | otherwise = return $ map ((,) (targetPos t)) inputs
    getPhonyInputs t = badPhony t "invalid"
    (phonyTargets, regularTargets) = partition ((".PHONY" `elem`) . targetOutputs) targets

RELEASE_INLINE(properEof)
properEof :: Monad m => Parser m ()
properEof = do
  P.eof
  state <- P.getState
  unless (null (stateIncludeStack state)) $
    fail "EOF but includers still pending"
  case stateLocalsStack state of
    [] -> return ()
    stack -> fail $ "EOF: unterminated locals: " ++ show stack

data OverrideVar = OverrideVar | DontOverrideVar

RELEASE_INLINE(varAssignment)
varAssignment :: Monad m => Parser m ()
varAssignment = do
  (varName, overrideVar) <-
      do x <- P.try ident
         horizSpaces
         y <-
           (OverrideVar     <$ P.string "=") <|>
           (DontOverrideVar <$ P.string "?=")
         horizSpaces
         return (x, y)
  value <- BS8.pack <$> P.many (unescapedChar <|> P.noneOf "#\t\n")
  case overrideVar of
    OverrideVar -> return ()
    DontOverrideVar -> tellWeakVar varName value
  skipLineSuffix
  let f DontOverrideVar (Just old) = Just old
      f _ _ = Just value
  whenCondTrue $ P.updateState $ atStateVars $
    M.alter (f overrideVar) varName

RELEASE_INLINE(directive)
directive :: Monad m => String -> Parser m a -> Parser m a
directive str act = do
  P.try $ P.optional (P.char ' ' *> horizSpaces) *> P.string str *> newWord
  horizSpaces *> act <* skipLineSuffix

RELEASE_INLINE(echoDirective)
echoDirective :: MonadMakefileParser m => Parser m ()
echoDirective = directive "echo" $ do
  pos <- P.getPosition
  str <- cartesian pos <$> interpolateVariables unescapedSequence "#\n"
  whenCondTrue $ lift $ MakefileMonad.outPutStrLn $ "ECHO: " <> str

RELEASE_INLINE(localsOpen)
localsOpen :: Monad m => Parser m ()
localsOpen = void $ P.updateState $ \state -> state { stateLocalsStack = stateVars state : stateLocalsStack state }

RELEASE_INLINE(localsClose)
localsClose :: Monad m => Parser m ()
localsClose = do
  state <- P.getState
  case stateLocalsStack state of
    [] -> fail "Close of locals without open"
    (oldVars:rest) ->
      P.putState $ state { stateLocalsStack = rest, stateVars = oldVars }

RELEASE_INLINE(localDirective)
localDirective :: Monad m => Parser m ()
localDirective =
  directive "local" $
  P.choice
    [ P.char '{' *> localsOpen
    , P.char '}' *> localsClose
    ]

data IfType = IfEq | IfNeq

RELEASE_INLINE(ifeqDirective)
ifeqDirective :: Monad m => String -> IfType -> Parser m ()
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

RELEASE_INLINE(endifDirective)
endifDirective :: Monad m => Parser m ()
endifDirective =
  directive "endif" $ updateConds $ CondState.unnest "endif mismatched"

RELEASE_INLINE(elseDirective)
elseDirective :: Monad m => Parser m ()
elseDirective =
  directive "else" $ updateConds (CondState.inverse "else mismatched")

RELEASE_INLINE(conditionalStatement)
conditionalStatement :: Monad m => Parser m ()
conditionalStatement = P.choice
  [ ifeqDirective "ifeq" IfEq
  , ifeqDirective "ifneq" IfNeq
  , elseDirective
  , endifDirective
  ]

RELEASE_INLINE(makefile)
makefile :: MonadMakefileParser m => Parser m Makefile
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

rethrow :: MonadMakefileParser m => (err -> String) -> m (Either err b) -> m b
rethrow errToStr act = do
  res <- act
  case res of
    Right x -> return x
    Left err -> do
      MakefileMonad.errPutStrLn $ BS8.pack $ errToStr err
      fail "Makefile parse failure"

RELEASE_INLINE(parse)
parse :: MonadMakefileParser m => FilePath -> Vars -> m Makefile
parse absMakefilePath vars =
  rethrow showErr $ join $ do
    input <- rethrow show $ MakefileMonad.tryReadFile absMakefilePath
    return $ P.runParserT makefile initialState (BS8.unpack absMakefilePath) input
  where
    initialState =
      State
      { stateIncludeStack = []
      , stateLocalsStack = []
      , stateRootDir = FilePath.takeDirectory absMakefilePath
      , stateVars = vars
      , stateCond = CondState.empty
      , stateWriter = mempty
      }
