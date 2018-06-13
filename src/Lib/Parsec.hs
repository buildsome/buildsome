{-# OPTIONS -fno-warn-orphans #-}
module Lib.Parsec (showErr, showPos) where


import Prelude.Compat hiding (FilePath)

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import qualified Text.Parsec.Error as ParseError
import qualified Text.Parsec.Pos as ParsecPos

instance Binary ParsecPos.SourcePos where
  put pos = do
    put $ ParsecPos.sourceName pos
    put $ ParsecPos.sourceLine pos
    put $ ParsecPos.sourceColumn pos
  get = mkPos <$> get <*> get <*> get
    where
      mkPos name line column =
        flip ParsecPos.setSourceColumn column $
        flip ParsecPos.setSourceLine line $
        ParsecPos.initialPos name
  {-# INLINE get #-}
  {-# INLINE put #-}

instance NFData ParsecPos.SourcePos where
  rnf pos =
    rnf (ParsecPos.sourceName pos) `seq`
    rnf (ParsecPos.sourceLine pos) `seq`
    rnf (ParsecPos.sourceColumn pos)
  {-# INLINE rnf #-}

showPos :: ParsecPos.SourcePos -> String
showPos pos = concat [path, ":", show line, ":", show col]
  where
    col = ParsecPos.sourceColumn pos
    line = ParsecPos.sourceLine pos
    path = ParsecPos.sourceName pos

showErr :: ParseError.ParseError -> String
showErr err = concat [showPos pos, ":", str]
  where
    pos = ParseError.errorPos err
    msgs = ParseError.errorMessages err
    str = ParseError.showErrorMessages
          "or" "unknown parse error"
          "expecting" "unexpected" "end of input"
          msgs
