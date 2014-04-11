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
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lib.Directory (getMFileStatus, catchDoesNotExist, getDirectoryContents)
import Lib.FilePath (FilePath)
import Lib.Posix.FileType (FileType, fileTypeOfStat)
import Lib.Posix.Instances ()
import Lib.TimeInstances ()
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

-- Must not compare other fields in the stat of directories because
-- they may change as files are created by various process in the
-- directory (see KNOWN_ISSUES).
data BasicStatEssence = BasicStatEssence
  { deviceID        :: Posix.DeviceID
  , fileGroup       :: Posix.GroupID
  , fileID          :: Posix.FileID
  , fileMode        :: Posix.FileMode
  , fileOwner       :: Posix.UserID
  , linkCount       :: Posix.LinkCount
  , specialDeviceID :: Posix.DeviceID
  } deriving (Generic, Eq, Show)
instance Binary BasicStatEssence

data FullStatEssence = FullStatEssence
  { basicStatEssence      :: BasicStatEssence
  , fileSize              :: Posix.FileOffset
  , fileType              :: FileType
  , accessTimeHiRes       :: POSIXTime
  , modificationTimeHiRes :: POSIXTime
  , statusChangeTimeHiRes :: POSIXTime
  } deriving (Generic, Eq, Show)
instance Binary FullStatEssence

data FileStatDesc
  = FileStatDirectory BasicStatEssence
  | FileStatOther FullStatEssence
  | NoFileStat
  deriving (Generic, Eq, Show)
instance Binary FileStatDesc

instance Binary Posix.CMode where
  get = Posix.CMode <$> getWord32le
  put (Posix.CMode x) = putWord32le x

data ThirdPartyMeddlingError = ThirdPartyMeddlingError FilePath String deriving (Show, Typeable)
instance E.Exception ThirdPartyMeddlingError

data UnsupportedFileTypeError = UnsupportedFileTypeError FilePath deriving (Show, Typeable)
instance E.Exception UnsupportedFileTypeError

-- Basic stat essence compares only things that do not change in a
-- directory stat when files are changed/created in that directory
basicStatEssenceOfStat :: Posix.FileStatus -> BasicStatEssence
basicStatEssenceOfStat stat = BasicStatEssence
  { deviceID          = Posix.deviceID stat
  , fileGroup         = Posix.fileGroup stat
  , fileID            = Posix.fileID stat
  , fileMode          = Posix.fileMode stat
  , fileOwner         = Posix.fileOwner stat
  , linkCount         = Posix.linkCount stat
  , specialDeviceID   = Posix.specialDeviceID stat
  }

-- Full stat essence should contain everything a program doing stat()
-- could be looking at
fullStatEssenceOfStat :: Posix.FileStatus -> FullStatEssence
fullStatEssenceOfStat stat = FullStatEssence
  { basicStatEssence = basicStatEssenceOfStat stat
  , fileType = fileTypeOfStat stat
  , fileSize = Posix.fileSize stat
  , accessTimeHiRes = Posix.accessTimeHiRes stat
  , modificationTimeHiRes = Posix.modificationTimeHiRes stat
  , statusChangeTimeHiRes = Posix.statusChangeTimeHiRes stat
  }

fileStatDesc :: Maybe Posix.FileStatus -> FileStatDesc
fileStatDesc Nothing = NoFileStat
fileStatDesc (Just stat)
  | Posix.isDirectory stat = FileStatDirectory $ basicStatEssenceOfStat stat
  | otherwise = FileStatOther $ fullStatEssenceOfStat stat

fileStatDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileStatDesc
fileStatDescOfMStat path oldMStat = do
  newMStat <- getMFileStatus path
  let oldMStatDesc = fileStatDesc oldMStat
      newMStatDesc = fileStatDesc newMStat
  when (oldMStatDesc /= newMStatDesc) $ E.throwIO $
    ThirdPartyMeddlingError path $ concat
    ["changed during build: ", show oldMStatDesc, " -> ", show newMStatDesc]
  return newMStatDesc

getFileStatDesc :: FilePath -> IO FileStatDesc
getFileStatDesc path = fileStatDescOfMStat path =<< getMFileStatus path

fileModeDescOfMStat :: FilePath -> Maybe Posix.FileStatus -> IO FileModeDesc
fileModeDescOfMStat path oldMStat = do
  newMStat <- getMFileStatus path
  let oldMMode = Posix.fileMode <$> oldMStat
      newMMode = Posix.fileMode <$> newMStat
  when (oldMMode /= newMMode) $ E.throwIO $
    ThirdPartyMeddlingError path $ concat
    ["mode changed during build: ", show oldMMode, " -> ", show newMMode]
  return $ maybe NoFileMode FileModeDesc newMMode

assertSameMTimes :: FilePath -> Maybe Posix.FileStatus -> Maybe Posix.FileStatus -> IO ()
assertSameMTimes path oldMStat newMStat =
  when (oldMMTime /= newMMTime) $ E.throwIO $
  ThirdPartyMeddlingError path $ concat
  ["changed during build: ", show oldMMTime, " -> ", show newMMTime]
  where
    oldMMTime = Posix.modificationTime <$> oldMStat
    newMMTime = Posix.modificationTime <$> newMStat

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
