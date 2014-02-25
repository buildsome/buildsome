{-# LANGUAGE DeriveGeneric #-}
module Lib.FileDesc
  ( FileDesc(..)
  , fileDescOfMStat
  , getFileDesc
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Lib.Directory (getMFileStatus, catchDoesNotExist)
import System.Posix.Files (FileStatus, isRegularFile, isDirectory, isSymbolicLink, modificationTime, readSymbolicLink)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.Directory as Dir

type ContentHash = ByteString

data FileDesc
  = RegularFile ContentHash
  | Symlink FilePath
  | Directory ContentHash -- Of the getDirectoryContents
  | NoFile -- an unlinked/deleted file at a certain path is also a
           -- valid input or output of a build step
  deriving (Generic, Eq, Show)
instance Binary FileDesc

fileDescOfMStat :: FilePath -> Maybe FileStatus -> IO FileDesc
fileDescOfMStat path oldMStat = do
  mContentHash <-
    case oldMStat of
    Just stat
      | isRegularFile stat ->
        Just . MD5.hash <$>
        assertExists (BS.readFile path)
      | isDirectory stat ->
        Just . MD5.hash . BS8.pack . unlines <$>
        assertExists (Dir.getDirectoryContents path)
    _ -> return Nothing
  -- Verify file did not change since we took its first mtime:
  newMStat <- getMFileStatus path
  when (not (compareMTimes oldMStat newMStat)) $ fail $
    show path ++ " changed during build!"
  case newMStat of
    Nothing -> return NoFile
    Just stat
      | isRegularFile stat ->
        return $ RegularFile $
        fromMaybe (error ("File disappeared: " ++ show path))
        mContentHash
      | isDirectory stat ->
        return $ Directory $
        fromMaybe (error ("Directory disappeared: " ++ show path))
        mContentHash
      | isSymbolicLink stat -> Symlink <$> readSymbolicLink path
      | otherwise -> fail $ "Unsupported file type: " ++ show path
  where
    assertExists act =
      act `catchDoesNotExist` fail (show path ++ " deleted during build!")
    compareMTimes x y =
      (modificationTime <$> x) ==
      (modificationTime <$> y)

getFileDesc :: FilePath -> IO FileDesc
getFileDesc path = fileDescOfMStat path =<< getMFileStatus path
