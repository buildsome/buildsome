{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Lib.FileDesc
  ( FileDesc
  , fileDescOfMStat
  , FileModeDesc
  , fileModeDescOfMStat
  , getFileModeDesc
  , assertSameMTimes
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lib.Directory (getMFileStatus, catchDoesNotExist, getDirectoryContents)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified System.Posix.ByteString as Posix

type ContentHash = ByteString

data FileDesc
  = RegularFile ContentHash
  | Symlink FilePath
  | Directory ContentHash -- Of the getDirectoryContents
  | NoFile -- an unlinked/deleted file at a certain path is also a
           -- valid input or output of a build step
  deriving (Generic, Eq, Show)
instance Binary FileDesc

data FileModeDesc = FileModeDesc Posix.FileMode | NoFileMode
  deriving (Generic, Eq, Show)
instance Binary FileModeDesc

instance Binary Posix.CMode where
  get = Posix.CMode <$> getWord32le
  put (Posix.CMode x) = putWord32le x

data ThirdPartyMeddlingError = ThirdPartyMeddlingError FilePath String deriving (Show, Typeable)
instance E.Exception ThirdPartyMeddlingError

data UnsupportedFileTypeError = UnsupportedFileTypeError FilePath deriving (Show, Typeable)
instance E.Exception UnsupportedFileTypeError

fileModeDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileModeDesc
fileModeDescOfMStat path oldMStat = do
  newMStat <- getMFileStatus path
  when ((Posix.fileMode <$> newMStat) /= (Posix.fileMode <$> oldMStat)) $ E.throwIO $
    ThirdPartyMeddlingError path "mode changed during build!"
  case newMStat of
    Nothing -> return NoFileMode
    Just stat -> return $ FileModeDesc $ Posix.fileMode stat

assertSameMTimes :: FilePath -> Maybe Posix.FileStatus -> Maybe Posix.FileStatus -> IO ()
assertSameMTimes path oldMStat newMStat =
  unless (compareMTimes oldMStat newMStat) $ E.throwIO $
  ThirdPartyMeddlingError path "changed during build!"
  where
    compareMTimes x y =
      (Posix.modificationTime <$> x) ==
      (Posix.modificationTime <$> y)

fileDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileDesc
fileDescOfMStat path oldMStat = do
  mContentHash <-
    case oldMStat of
    Just stat
      | Posix.isRegularFile stat ->
        Just . MD5.hash <$>
        assertExists (BS8.readFile (BS8.unpack path))
      | Posix.isDirectory stat ->
        Just . MD5.hash . BS8.unlines <$>
        assertExists (getDirectoryContents path)
    _ -> return Nothing
  -- Verify file did not change since we took its first mtime:
  newMStat <- getMFileStatus path
  assertSameMTimes path oldMStat newMStat
  case newMStat of
    Nothing -> return NoFile
    Just stat
      | Posix.isRegularFile stat ->
        return $ RegularFile $
        fromMaybe (error ("File disappeared: " ++ show path))
        mContentHash
      | Posix.isDirectory stat ->
        return $ Directory $
        fromMaybe (error ("Directory disappeared: " ++ show path))
        mContentHash
      | Posix.isSymbolicLink stat -> Symlink <$> Posix.readSymbolicLink path
      | otherwise -> E.throwIO $ UnsupportedFileTypeError path
  where
    assertExists act =
      act `catchDoesNotExist`
      E.throwIO (ThirdPartyMeddlingError path "deleted during build!")

getFileModeDesc :: FilePath -> IO FileModeDesc
getFileModeDesc path = fileModeDescOfMStat path =<< getMFileStatus path
