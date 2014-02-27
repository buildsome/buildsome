module Lib.Parsec (parseFromFile) where

import Data.Monoid
import qualified Text.Parsec as P

parseFromFile ::
  (Monad m, Monoid u) =>
  P.ParsecT String u m a -> FilePath -> IO (m (Either P.ParseError a))
parseFromFile p fname = do
  input <- readFile fname
  return $ P.runParserT p mempty fname input
