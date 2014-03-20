module Lib.Directory
  ( getMFileStatus
  , catchDoesNotExist
  , removeFileOrDirectory
  , removeFileOrDirectoryOrNothing
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import System.IO.Error
import System.Posix.Files (FileStatus, getFileStatus, fileExist)
import qualified Control.Exception as E
import qualified System.Directory as Dir

catchDoesNotExist :: IO a -> IO a -> IO a
catchDoesNotExist act handler =
  E.catchJust predicate act $ \() -> handler
  where
    predicate e
      | isDoesNotExistErrorType (ioeGetErrorType e) = Just ()
      | otherwise = Nothing

getMFileStatus :: FilePath -> IO (Maybe FileStatus)
getMFileStatus path = do
  exists <- fileExist path
  if exists
    then (Just <$> getFileStatus path) `catchDoesNotExist` return Nothing
    else return Nothing

removeFileOrDirectoryOrNothing :: FilePath -> IO ()
removeFileOrDirectoryOrNothing path = do
  f <- Dir.doesFileExist path
  d <- Dir.doesDirectoryExist path
  when f $ Dir.removeFile path
  when d $ Dir.removeDirectoryRecursive path

removeFileOrDirectory :: FilePath -> IO ()
removeFileOrDirectory path = do
  d <- Dir.doesDirectoryExist path
  if d then Dir.removeDirectoryRecursive path
       else Dir.removeFile path
