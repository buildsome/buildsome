{-# LANGUAGE BangPatterns #-}
module Lib.Directory
  ( getMFileStatus
  , catchDoesNotExist
  , removeFileOrDirectory
  , removeFileOrDirectoryOrNothing
  , createDirectories
  , getDirectoryContents
  , getDirectoryContentsHash
  , makeAbsolutePath
  ) where

import qualified Control.Exception as E
import           Control.Monad
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid ((<>))
import           Lib.Exception (bracket)
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import qualified System.Directory as Dir
import           System.IO.Error
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath)

catchErrorPred :: (IOErrorType -> Bool) -> IO a -> IO a -> IO a
catchErrorPred predicate act handler =
  act `E.catch` \e ->
  if predicate (ioeGetErrorType e)
  then handler
  else E.throwIO e

catchDoesNotExist :: IO a -> IO a -> IO a
catchDoesNotExist = catchErrorPred isDoesNotExistErrorType

catchAlreadyExists :: IO a -> IO a -> IO a
catchAlreadyExists = catchErrorPred isAlreadyExistsErrorType

getMFileStatus :: FilePath -> IO (Maybe Posix.FileStatus)
getMFileStatus path = do
  doesExist <- FilePath.exists path
  if doesExist
    then (Just <$> Posix.getFileStatus path) `catchDoesNotExist` pure Nothing
    else pure Nothing

createDirectories :: FilePath -> IO ()
createDirectories path
  | BS8.null path = pure ()
  | otherwise = do
    doesExist <- FilePath.exists path
    unless doesExist $ do
      createDirectories $ FilePath.takeDirectory path
      Posix.createDirectory path 0o777 `catchAlreadyExists` pure () 

removeFileByStat :: IO () -> FilePath -> IO ()
removeFileByStat notExist path = do
  mFileStat <- getMFileStatus path
  case mFileStat of
    Nothing -> notExist
    Just fileStat
      | Posix.isRegularFile  fileStat -> Posix.removeLink path
      | Posix.isSymbolicLink fileStat -> Posix.removeLink path
      | Posix.isDirectory    fileStat -> Dir.removeDirectoryRecursive $ BS8.unpack path
      | otherwise -> error $ "removeFileOrDirectoryOrNothing: unsupported filestat " ++ show path

removeFileOrDirectoryOrNothing :: FilePath -> IO ()
removeFileOrDirectoryOrNothing = removeFileByStat $ pure ()

removeFileOrDirectory :: FilePath -> IO ()
removeFileOrDirectory path =
  removeFileByStat
  -- Try to remove the file when it doesn't exist in order to generate
  -- the meaningful IO exception:
  (Posix.removeLink path) path

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  bracket (Posix.openDirStream path) Posix.closeDirStream go
  where
    go dirStream = do
      fn <- Posix.readDirStream dirStream
      if BS8.null fn
        then pure []
        else (fn :) <$> go dirStream

getDirectoryContentsHash :: FilePath -> IO BS8.ByteString
getDirectoryContentsHash path =
  bracket (Posix.openDirStream path) Posix.closeDirStream (go BS8.empty)
  where
    go !hash !dirStream = do
      fn <- Posix.readDirStream dirStream
      if BS8.null fn
        then pure hash
        else go (MD5.hash (hash <> fn)) dirStream

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath path = (</> path) <$> Posix.getWorkingDirectory
