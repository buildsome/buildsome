module Lib.Directory (getMFileStatus, fileExists, catchDoesNotExist) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import System.IO.Error
import System.Posix.Files (FileStatus, getFileStatus, isRegularFile, modificationTime)
import qualified Control.Exception as E

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
