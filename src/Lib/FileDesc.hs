{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, NoMonomorphismRestriction, OverloadedStrings #-}
module Lib.FileDesc
  ( FileContentDesc(..)
  , fileContentDescOfStat

  , FileModeDesc(..)
  , fileModeDescOfStat

  , FileStatDesc(..)
  , fileStatDescOfStat
  ) where


import Prelude.Compat hiding (FilePath)

import Data.Binary (Binary(..))
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lib.Cmp (Cmp(..), cmpGetterBy)
import Lib.FilePath (FilePath)
import Lib.Posix.FileType (FileType, fileTypeOfStat)
import Lib.Posix.Instances ()
import Lib.TimeInstances ()
import qualified Control.Exception as E
import qualified Lib.Hash as Hash
import           Lib.Hash (Hash)
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.Cmp as Cmp
import qualified Lib.Directory as Dir
import qualified System.Posix.ByteString as Posix

type ContentHash = Hash

data FileContentDesc
  = FileContentDescRegular ContentHash
  | FileContentDescSymlink FilePath
  | FileContentDescDir ContentHash -- Of the getDirectoryContents
  deriving (Generic, Eq, Show)
instance Binary FileContentDesc
instance Cmp FileContentDesc where
  FileContentDescRegular x `cmp` FileContentDescRegular y = Cmp.eq ["change"] x y
  FileContentDescSymlink x `cmp` FileContentDescSymlink y = map ("symlink target: " <>) <$> Cmp.eqShow x y
  FileContentDescDir x `cmp` FileContentDescDir y = Cmp.eq ["dir listing changed"] x y
  FileContentDescRegular _ `cmp` _ = Cmp.NotEquals ["regular file vs. non-regular"]
  FileContentDescSymlink _ `cmp` _ = Cmp.NotEquals ["symlink vs. non-symlink"]
  FileContentDescDir _ `cmp` _ = Cmp.NotEquals ["dir vs. non-dir"]

data FileModeDesc = FileModeDesc Posix.FileMode
  deriving (Generic, Eq, Show)
instance Binary FileModeDesc
instance Cmp FileModeDesc where
  FileModeDesc x `cmp` FileModeDesc y = Cmp.eqShow x y

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
instance Cmp BasicStatEssence where
  cmp =
    mconcat
    [ cShow "devID" deviceID
    , cShow "sdevID" specialDeviceID
    , cShow "uid" fileOwner
    , cShow "gid" fileGroup
    , cShow "fileMode" fileMode
    ]
    where
      cShow = cmpGetterBy Cmp.eqShow

data FullStatEssence = FullStatEssence
  { basicStatEssence      :: BasicStatEssence
  , fileSize              :: Posix.FileOffset
  , fileType              :: FileType
  -- Tracking access time is meaningless
  , modificationTimeHiRes :: POSIXTime
  , statusChangeTimeHiRes :: POSIXTime
  } deriving (Generic, Eq, Show)
instance Binary FullStatEssence
instance Cmp FullStatEssence where
  cmp =
    mconcat
    [ cmp `on` basicStatEssence
    , cShow "size" fileSize
    , cShow "type" fileType
    ]
    where
      cShow = cmpGetterBy Cmp.eqShow

data FileStatDesc
  = FileStatDirectory BasicStatEssence
  | FileStatOther FullStatEssence
  deriving (Generic, Eq, Show)
instance Binary FileStatDesc
instance Cmp FileStatDesc where
  FileStatDirectory a `cmp` FileStatDirectory b = cmp a b
  FileStatOther a `cmp` FileStatOther b = cmp a b
  FileStatOther _ `cmp` FileStatDirectory _ = Cmp.NotEquals ["Non-directory vs. Directory"]
  FileStatDirectory _ `cmp` FileStatOther _ = Cmp.NotEquals ["Directory vs. Non-directory"]

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
    FileContentDescRegular . Hash.md5 <$> BS8.readFile (BS8.unpack path)
  | Posix.isDirectory stat =
    FileContentDescDir <$> Dir.getDirectoryContentsHash path
  | Posix.isSymbolicLink stat =
    FileContentDescSymlink <$> Posix.readSymbolicLink path
  | otherwise = E.throwIO $ UnsupportedFileTypeError path
