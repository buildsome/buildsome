module Lib.FilePath
  ( splitFileName
  , canonicalizePath
  , canonicalizeAbsPath
  , (</>)
  ) where

import qualified System.Directory as Dir
import qualified System.FilePath as FilePath

removeRedundantComponents :: FilePath -> FilePath
removeRedundantComponents =
  FilePath.joinPath .
  foldr step [] .
  filter (/= ".") .
  map FilePath.dropTrailingPathSeparator .
  FilePath.splitPath
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

canonicalizeAbsPath :: FilePath -> IO FilePath
canonicalizeAbsPath path =
  Dir.makeRelativeToCurrentDirectory $ removeRedundantComponents path

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  curDir <- Dir.getCurrentDirectory
  canonicalizeAbsPath (curDir </> path)
