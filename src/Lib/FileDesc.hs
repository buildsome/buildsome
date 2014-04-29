{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Lib.FileDesc
  ( FileContentDesc
  , fileContentDescOfStat

  , FileModeDesc
  , fileModeDescOfStat

  , FileStatDesc
  , fileStatDescOfStat
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lib.FilePath (FilePath)
import Lib.Posix.FileType (FileType, fileTypeOfStat)
import Lib.Posix.Instances ()
import Lib.TimeInstances ()
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.Directory as Dir
import qualified System.Posix.ByteString as Posix

type ContentHash = ByteString

data FileContentDesc
  = FileContentDescRegular ContentHash
  | FileContentDescSymlink FilePath
  | FileContentDescDir ContentHash -- Of the getDirectoryContents
  deriving (Generic, Eq, Show)
instance Binary FileContentDesc

data FileModeDesc = FileModeDesc Posix.FileMode
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
  , specialDeviceID :: Posix.DeviceID
  } deriving (Generic, Eq, Show)
instance Binary BasicStatEssence

data FullStatEssence = FullStatEssence
  { basicStatEssence      :: BasicStatEssence
  , fileSize              :: Posix.FileOffset
  , fileType              :: FileType
  -- Tracking access time is meaningless
  , modificationTimeHiRes :: POSIXTime
  , statusChangeTimeHiRes :: POSIXTime
  } deriving (Generic, Eq, Show)
instance Binary FullStatEssence

data FileStatDesc
  = FileStatDirectory BasicStatEssence
  | FileStatOther FullStatEssence
  deriving (Generic, Eq, Show)
instance Binary FileStatDesc

instance Binary Posix.CMode where
  get = Posix.CMode <$> get
  put (Posix.CMode x) = put x

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
  , specialDeviceID   = Posix.specialDeviceID stat
  }

-- Full stat essence should contain everything a program doing stat()
-- could be looking at
fullStatEssenceOfStat :: Posix.FileStatus -> FullStatEssence
fullStatEssenceOfStat stat = FullStatEssence
  { basicStatEssence = basicStatEssenceOfStat stat
  , fileType = fileTypeOfStat stat
  , fileSize = Posix.fileSize stat
  , modificationTimeHiRes = Posix.modificationTimeHiRes stat
  , statusChangeTimeHiRes = Posix.statusChangeTimeHiRes stat
  }

fileStatDescOfStat :: Posix.FileStatus -> FileStatDesc
fileStatDescOfStat stat
  | Posix.isDirectory stat = FileStatDirectory $ basicStatEssenceOfStat stat
  | otherwise = FileStatOther $ fullStatEssenceOfStat stat

fileModeDescOfStat :: Posix.FileStatus -> FileModeDesc
fileModeDescOfStat = FileModeDesc . Posix.fileMode

fileContentDescOfStat :: FilePath -> Posix.FileStatus -> IO FileContentDesc
fileContentDescOfStat path stat
  | Posix.isRegularFile stat =
    FileContentDescRegular . MD5.hash <$> BS8.readFile (BS8.unpack path)
  | Posix.isDirectory stat =
    FileContentDescDir . MD5.hash . BS8.unlines <$> Dir.getDirectoryContents path
  | Posix.isSymbolicLink stat =
    FileContentDescSymlink <$> Posix.readSymbolicLink path
  | otherwise = E.throwIO $ UnsupportedFileTypeError path
