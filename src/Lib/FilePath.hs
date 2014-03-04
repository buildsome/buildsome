module Lib.FilePath (removeRedundantParents, splitFileName, (</>)) where

import qualified System.FilePath as FilePath

removeRedundantParents :: FilePath -> FilePath
removeRedundantParents = FilePath.joinPath . go . FilePath.splitPath
  where
    go [] = []
    go (x:xs) =
      case go xs of
      "../":rest -> rest
      "..":rest -> rest
      rest -> x:rest

splitFileName :: FilePath -> (FilePath, String)
splitFileName path = (FilePath.dropTrailingPathSeparator dir, file)
  where
    (dir, file) = FilePath.splitFileName path

(</>) :: FilePath -> FilePath -> FilePath
"." </> y = y
x </> y = x FilePath.</> y