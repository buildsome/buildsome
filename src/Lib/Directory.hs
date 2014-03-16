module Lib.Directory
  ( getMFileStatus
  , fileExists, catchDoesNotExist
  , removeFileOrDirectory
  , removeFileOrDirectoryOrNothing
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe (isJust)
import System.IO.Error
import System.Posix.Files (FileStatus, getFileStatus)
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
getMFileStatus path =
  (Just <$> getFileStatus path)
  `catchDoesNotExist` return Nothing

fileExists :: FilePath -> IO Bool
fileExists path = isJust <$> getMFileStatus path

removeFileOrDirectoryOrNothing :: FilePath -> IO ()
removeFileOrDirectoryOrNothing path = do
  f <- Dir.doesFileExist path
  d <- Dir.doesDirectoryExist path
  when f $ Dir.removeFile path
  when d $ Dir.removeDirectoryRecursive path

removeFileOrDirectory :: FilePath -> IO ()
removeFileOrDirectory path = do
  f <- Dir.doesFileExist path
  d <- Dir.doesDirectoryExist path
  case (f, d) of
    (True, True) -> fail "doesFileExist && doesDirectoryExist both true?!"
    (True, False) -> Dir.removeFile path
    (False, True) -> Dir.removeDirectoryRecursive path
    (False, False) -> fail $ path ++ " does not exist"
