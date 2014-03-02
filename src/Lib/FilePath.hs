module Lib.FilePath (splitFileName) where

import qualified System.FilePath as FilePath

splitFileName :: FilePath -> (FilePath, String)
splitFileName path = (FilePath.dropTrailingPathSeparator dir, file)
  where
    (dir, file) = FilePath.splitFileName path
