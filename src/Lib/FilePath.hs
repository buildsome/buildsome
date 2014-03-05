module Lib.FilePath (removeRedundantParents, splitFileName, (</>), canonicalizePath) where

import qualified System.Directory as Dir
import qualified System.FilePath as FilePath

removeRedundantParents :: FilePath -> FilePath
removeRedundantParents =
  FilePath.joinPath . foldr step [] . map FilePath.dropTrailingPathSeparator . FilePath.splitPath
  where
    step "/" xs = "/" : xs
    step ".." xs = ".." : xs
    step _ ("..":xs) = xs
    step x xs = x:xs

splitFileName :: FilePath -> (FilePath, String)
splitFileName path = (FilePath.dropTrailingPathSeparator dir, file)
  where
    (dir, file) = FilePath.splitFileName path

(</>) :: FilePath -> FilePath -> FilePath
"." </> y = y
x </> y = x FilePath.</> y

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  curDir <- Dir.getCurrentDirectory
  Dir.makeRelativeToCurrentDirectory $ removeRedundantParents (curDir </> path)
