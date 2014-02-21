{-# OPTIONS -Wall -O2 #-}
module Lib.Protocol
  ( parseMsg
  , OpenMode(..), showOpenMode
  , CreationMode(..), showCreationMode
  , Func(..), showFunc
  ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.IntMap (IntMap, (!))
import Data.Word
import Lib.ByteString (truncateAt)
import Numeric (showOct)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M

data OpenMode = OpenReadMode | OpenWriteMode
  deriving (Show)
showOpenMode :: OpenMode -> String
showOpenMode OpenReadMode = "R"
showOpenMode OpenWriteMode = "W"

data CreationMode = NoCreate | Create Word32 -- Unix permissions
  deriving (Show)
showCreationMode :: CreationMode -> String
showCreationMode NoCreate = ""
showCreationMode (Create x) = "(CREATE:" ++ showOct x "" ++ ")"

data Func
  = Open FilePath OpenMode CreationMode
  | Stat FilePath
  | LStat FilePath
  | Creat FilePath Word32
  | Rename FilePath FilePath
  | Unlink FilePath
  | Access FilePath Word32{- TODO: replace Int with AccessMode -}
  | OpenDir FilePath
  | Truncate FilePath Word64{- length -}
  | Chmod FilePath Word32{-mode-}
  | ReadLink FilePath
  | MkNod FilePath Word32{-mode-} Word64{-dev-}
  | MkDir FilePath Word32{-mode-}
  | RmDir FilePath
  | SymLink FilePath FilePath
  | Link FilePath FilePath
  | Chown FilePath Word32 Word32
  deriving (Show)

showFunc :: Func -> String
showFunc (Open path mode creation) = "open:" ++ showOpenMode mode ++ show path ++ showCreationMode creation
showFunc (Stat path) = "stat:" ++ show path
showFunc (LStat path) = "lstat:" ++ show path
showFunc (Creat path perms) = concat ["create:", show path, " ", showOct perms ""]
showFunc (Rename old new) = concat ["rename:", show old, "->", show new]
showFunc (Unlink path) = concat ["unlink:", show path]
showFunc (Access path mode) = concat ["access:", show path, " ", show mode]
showFunc (OpenDir path) = concat ["openDir:", show path]
showFunc (Truncate path len) = concat ["truncate:", show path, " ", show len]
showFunc (Chmod path perms) = concat ["chmod:", show path, " ", showOct perms ""]
showFunc (ReadLink path) = concat ["readlink:", show path]
showFunc (MkNod path mode dev) = unwords ["mknod:", show path, showOct mode "", show dev]
showFunc (MkDir path mode) = unwords ["mkdir:", show path, showOct mode ""]
showFunc (RmDir path) = unwords ["rmdir:", show path]
showFunc (SymLink target linkpath) = unwords ["symlink:", show target, show linkpath]
showFunc (Link src dest) = unwords ["link:", show src, show dest]
showFunc (Chown path uid gid) = unwords ["chown:", show path, show uid, show gid]

mAX_PATH :: Int
mAX_PATH = 256

getPath :: Get FilePath
getPath = BS8.unpack . truncateAt 0 <$> getByteString mAX_PATH

fLAG_WRITE :: Word32
fLAG_WRITE = 1
fLAG_CREATE :: Word32
fLAG_CREATE = 2

parseOpen :: Get Func
parseOpen = mkOpen <$> getPath <*> getWord32le <*> getWord32le
  where
    mkOpen path flags mode = Open path (openMode flags) (creationMode flags mode)
    openMode flags
      | 0 /= flags .&. fLAG_WRITE = OpenWriteMode
      | otherwise = OpenReadMode
    creationMode flags mode
      | 0 /= flags .&. fLAG_CREATE = Create mode
      | otherwise = NoCreate

funcs :: IntMap (String, Get Func)
funcs =
  M.fromList
  [ (0x10000, ("open", parseOpen))
  , (0x10001, ("creat", Creat <$> getPath <*> getWord32le))
  , (0x10002, ("stat", Stat <$> getPath))
  , (0x10003, ("lstat", LStat <$> getPath))
  , (0x10004, ("opendir", OpenDir <$> getPath))
  , (0x10005, ("access", Access <$> getPath <*> getWord32le))
  , (0x10006, ("truncate", Truncate <$> getPath <*> getWord64le))
  , (0x10007, ("unlink", Unlink <$> getPath))
  , (0x10008, ("rename", Rename <$> getPath <*> getPath))
  , (0x10009, ("chmod", Chmod <$> getPath <*> getWord32le))
  , (0x1000A, ("readlink", ReadLink <$> getPath))
  , (0x1000B, ("mknod", MkNod <$> getPath <*> getWord32le <*> getWord64le))
  , (0x1000C, ("mkdir", MkDir <$> getPath <*> getWord32le))
  , (0x1000D, ("rmdir", RmDir <$> getPath))
  , (0x1000E, ("symlink", SymLink <$> getPath <*> getPath))
  , (0x1000F, ("link", Link <$> getPath <*> getPath))
  , (0x10010, ("chown", Chown <$> getPath <*> getWord32le <*> getWord32le))
  ]

parseMsgLazy :: BSL.ByteString -> Func
parseMsgLazy = runGet $ do
  funcId <- getWord32le
  let (_name, getter) = funcs ! fromIntegral funcId
  func <- getter
  finished <- isEmpty
  when (not finished) $ fail "Unexpected trailing input in message"
  return func

strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy x = BSL.fromChunks [x]

parseMsg :: BS.ByteString -> Func
parseMsg = parseMsgLazy . strictToLazy
