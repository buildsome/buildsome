module Lib.Parsec (parseFromFile) where

import qualified Text.Parsec as P

parseFromFile :: P.Parsec String () a -> FilePath -> IO (Either P.ParseError a)
parseFromFile p fname = do
  input <- readFile fname
  return $ P.runParser p () fname input
