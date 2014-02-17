{-# OPTIONS -Wall -O2 #-}
module Lib.Protocol
  ( parseMsg
  ) where

import Control.Applicative
import Data.Binary.Get
import Data.IntMap (IntMap, (!))
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M

type OpenMode = Word32
type OpenFlags = Word32

data Func
  = Open FilePath OpenFlags OpenMode
  deriving (Show)

mAX_PATH :: Int
mAX_PATH = 256

truncateAtZero :: BS.ByteString -> BS.ByteString
truncateAtZero bs =
  case BS.split 0 bs of
  [] -> BS.empty
  (x:_) -> x

getPath :: Get FilePath
getPath = BS8.unpack . truncateAtZero <$> getByteString mAX_PATH

parseOpen :: Get Func
parseOpen = Open <$> getPath <*> getWord32le <*> getWord32le

funcs :: IntMap (String, Get Func)
funcs =
  M.fromList
  [ (0x10000, ("open", parseOpen))
  -- , (0x10001, ("creat", parseCreat))
  -- , (0x10002, ("stat", parseStat))
  -- , (0x10003, ("lstat", parseLstat))
  -- , (0x10004, ("opendir", parseOpendir))
  -- , (0x10005, ("fdopendir", parseFdopendir))
  -- , (0x10006, ("access", parseAccess))
  -- , (0x10007, ("truncate", parseTruncate))
  -- , (0x10008, ("ftruncate", parseFtruncate))
  -- , (0x10009, ("unlink", parseUnlink))
  -- , (0x1000A, ("rename", parseRename))
  -- , (0x1000B, ("chmod", parseChmod))
  -- , (0x1000C, ("readlink", parseReadlink))
  -- , (0x1000D, ("mknod", parseMknod))
  -- , (0x1000E, ("mkdir", parseMkdir))
  -- , (0x1000F, ("rmdir", parseRmdir))
  -- , (0x10010, ("symlink", parseSymlink))
  -- , (0x10011, ("link", parseLink))
  -- , (0x10012, ("chown", parseChown))
  ]

parseMsgLazy :: BSL.ByteString -> Func
parseMsgLazy = runGet $ do
  funcId <- getWord32le
  let (_name, getter) = funcs ! fromIntegral funcId
  getter

strictToLazy :: BS.ByteString -> BSL.ByteString
strictToLazy x = BSL.fromChunks [x]

parseMsg :: BS.ByteString -> Func
parseMsg = parseMsgLazy . strictToLazy
