{-# LANGUAGE DeriveGeneric #-}
module Lib.Posix.FileType
  ( FileType(..), fileTypeOfStat
  ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified System.Posix.ByteString as Posix

data FileType
  = BlockDevice
  | CharacterDevice
  | Directory
  | NamedPipe
  | RegularFile
  | Socket
  | SymbolicLink
  deriving (Generic, Eq, Show, Ord)
instance Binary FileType

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
