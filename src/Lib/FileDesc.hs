{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Lib.FileDesc
  ( FileDesc
  , fileDescOfMStat

  , FileModeDesc
  , fileModeDescOfMStat, getFileModeDesc

  , assertSameMTimes

  , FileStatDesc
  , fileStatDescOfMStat, getFileStatDesc
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
import Lib.PosixInstances ()
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified System.Posix.ByteString as Posix

type ContentHash = ByteString

data FileDesc
  = RegularFileDesc ContentHash
  | SymlinkDesc FilePath
  | DirectoryDesc ContentHash -- Of the getDirectoryContents
  | NoFileDesc -- an unlinked/deleted file at a certain path is also a
           -- valid input or output of a build step
  deriving (Generic, Eq, Show)
instance Binary FileDesc

data FileModeDesc = FileModeDesc Posix.FileMode | NoFileMode
  deriving (Generic, Eq, Show)
instance Binary FileModeDesc

data FileType
  = BlockDevice
  | CharacterDevice
  | Directory
  | NamedPipe
  | RegularFile
  | Socket
  | SymbolicLink
  deriving (Generic, Eq, Show)
instance Binary FileType

data FileStatEssence = FileStatEssence
  { deviceID        :: Posix.DeviceID
  , fileGroup       :: Posix.GroupID
  , fileID          :: Posix.FileID
  , fileMode        :: Posix.FileMode
  , fileOwner       :: Posix.UserID
  , fileType        :: FileType
  , linkCount       :: Posix.LinkCount
  , specialDeviceID :: Posix.DeviceID
  , fileSize        :: Maybe Posix.FileOffset -- Nothing if FileType=Directory (see KNOWN_ISSUES (stat of directory))
  } deriving (Generic, Eq, Show)
instance Binary FileStatEssence

data FileStatDesc = FileStatDesc FileStatEssence | NoFileStat
  deriving (Generic, Eq, Show)
instance Binary FileStatDesc

instance Binary Posix.CMode where
  get = Posix.CMode <$> getWord32le
  put (Posix.CMode x) = putWord32le x

data ThirdPartyMeddlingError = ThirdPartyMeddlingError FilePath String deriving (Show, Typeable)
instance E.Exception ThirdPartyMeddlingError

data UnsupportedFileTypeError = UnsupportedFileTypeError FilePath deriving (Show, Typeable)
instance E.Exception UnsupportedFileTypeError

fileTypeOfStat :: Posix.FileStatus -> FileType
fileTypeOfStat stat
  | Posix.isBlockDevice stat = BlockDevice
  | Posix.isCharacterDevice stat = CharacterDevice
  | Posix.isDirectory stat = Directory
  | Posix.isNamedPipe stat = NamedPipe
  | Posix.isRegularFile stat = RegularFile
  | Posix.isSocket stat = Socket
  | Posix.isSymbolicLink stat = SymbolicLink
  | otherwise = error "Unrecognized file type"

fileStatEssence :: Posix.FileStatus -> FileStatEssence
fileStatEssence stat =
  FileStatEssence
  { deviceID          = Posix.deviceID stat
  , fileGroup         = Posix.fileGroup stat
  , fileID            = Posix.fileID stat
  , fileMode          = Posix.fileMode stat
  , fileOwner         = Posix.fileOwner stat
  , fileType          = fileTypeOfStat stat
  , linkCount         = Posix.linkCount stat
  , specialDeviceID   = Posix.specialDeviceID stat
  , fileSize          =
    if Posix.isDirectory stat
    then Nothing -- See KNOWN_ISSUES (stat of directory)
    else Just (Posix.fileSize stat)
  }

fileStatDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileStatDesc
fileStatDescOfMStat path oldMStat = do
  newMStat <- getMFileStatus path
  let newMStatEssence = fileStatEssence <$> newMStat
  when (newMStatEssence /= (fileStatEssence <$> oldMStat)) $ E.throwIO $
    ThirdPartyMeddlingError path "changed during build!"
  return $ maybe NoFileStat FileStatDesc newMStatEssence

getFileStatDesc :: FilePath -> IO FileStatDesc
getFileStatDesc path = fileStatDescOfMStat path =<< getMFileStatus path

fileModeDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileModeDesc
fileModeDescOfMStat path oldMStat = do
  newMStat <- getMFileStatus path
  let newMMode = Posix.fileMode <$> newMStat
  when (newMMode /= (Posix.fileMode <$> oldMStat)) $ E.throwIO $
    ThirdPartyMeddlingError path "mode changed during build!"
  return $ maybe NoFileMode FileModeDesc newMMode

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
    Nothing -> return NoFileDesc
    Just stat
      | Posix.isRegularFile stat ->
        return $ RegularFileDesc $
        fromMaybe (error ("File disappeared: " ++ show path))
        mContentHash
      | Posix.isDirectory stat ->
        return $ DirectoryDesc $
        fromMaybe (error ("Directory disappeared: " ++ show path))
        mContentHash
      | Posix.isSymbolicLink stat -> SymlinkDesc <$> Posix.readSymbolicLink path
      | otherwise -> E.throwIO $ UnsupportedFileTypeError path
  where
    assertExists act =
      act `catchDoesNotExist`
      E.throwIO (ThirdPartyMeddlingError path "deleted during build!")

getFileModeDesc :: FilePath -> IO FileModeDesc
getFileModeDesc path = fileModeDescOfMStat path =<< getMFileStatus path
