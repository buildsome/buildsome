module Lib.Parsec (parseFromFile, showErr, showPos) where

import Data.ByteString (ByteString)
import Lib.FilePath (FilePath, (</>))
import Prelude hiding (FilePath)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.Posix.ByteString as Posix
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as ParseError
import qualified Text.Parsec.Pos as ParsePos

parseFromFile ::
  Monad m => P.ParsecT ByteString u m a -> u -> FilePath -> IO (m (Either P.ParseError a))
parseFromFile p u fname = do
  input <- BS.readFile (BS8.unpack fname)
  cwd <- Posix.getWorkingDirectory
  return $ P.runParserT p u (BS8.unpack (cwd </> fname)) input

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
