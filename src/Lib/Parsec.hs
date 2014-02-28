module Lib.Parsec (parseFromFile, showErr, showPos) where

import Data.Monoid
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as ParseError
import qualified Text.Parsec.Pos as ParsePos

parseFromFile ::
  (Monad m, Monoid u) =>
  P.ParsecT String u m a -> FilePath -> IO (m (Either P.ParseError a))
parseFromFile p fname = do
  input <- readFile fname
  return $ P.runParserT p mempty fname input

showPos :: ParsePos.SourcePos -> String
showPos pos = concat [path, ":", show line, ":", show col]
  where
    col = ParsePos.sourceColumn pos
    line = ParsePos.sourceLine pos
    path = ParsePos.sourceName pos

showErr :: ParseError.ParseError -> String
showErr err = concat [showPos pos, ":", str]
  where
    pos = ParseError.errorPos err
    msgs = ParseError.errorMessages err
    str = ParseError.showErrorMessages
          "or" "unknown parse error"
          "expecting" "unexpected" "end of input"
          msgs
