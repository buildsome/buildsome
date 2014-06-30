module Lib.Parsec (showErr, showPos) where

import Prelude hiding (FilePath)
import qualified Text.Parsec.Error as ParseError
import qualified Text.Parsec.Pos as ParsePos

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
