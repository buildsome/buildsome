{-# LANGUAGE OverloadedStrings #-}
module Lib.FSHook.Protocol
  ( parseMsg
  , OpenMode(..), showOpenMode
  , CreationMode(..), showCreationMode
  , Func(..), showFunc
  ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import Data.IntMap (IntMap, (!))
import Data.Word
import Lib.ByteString (truncateAt)
import Lib.Directory (catchDoesNotExist)
import Lib.FilePath (FilePath, (</>))
import Numeric (showOct)
import Prelude hiding (FilePath)
import System.Posix.Files.ByteString (fileAccess)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M

data OpenMode = OpenReadMode | OpenWriteMode
  deriving (Show)
showOpenMode :: OpenMode -> String
showOpenMode OpenReadMode = "R"
showOpenMode OpenWriteMode = "W"
{-# INLINE showOpenMode #-}

data CreationMode = NoCreate | Create Word32 -- Unix permissions
  deriving (Show)
showCreationMode :: CreationMode -> String
showCreationMode NoCreate = ""
showCreationMode (Create x) = "(CREATE:" ++ showOct x "" ++ ")"
{-# INLINE showCreationMode #-}

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
  | Exec FilePath
  | ExecP (Maybe FilePath) [FilePath]{-prior searched paths (that did not exist)-}
  deriving (Show)

{-# INLINE showFunc #-}
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
showFunc (Exec path) = unwords ["exec:", show path]
showFunc (ExecP (Just path) attempted) = unwords ["execP:", show path, "searched:", show attempted]
showFunc (ExecP Nothing attempted) = unwords ["failedExecP:searched:", show attempted]

{-# ANN module ("HLint: ignore Use ++"::String) #-}
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

mAX_PATH :: Int
mAX_PATH = 256
mAX_PATH_ENV_VAR_LENGTH :: Int
mAX_PATH_ENV_VAR_LENGTH = 10*1024
mAX_PATH_CONF_STR :: Int
mAX_PATH_CONF_STR = 10*1024
mAX_EXEC_FILE :: Int
mAX_EXEC_FILE = mAX_PATH

getNullTerminated :: Int -> Get FilePath
getNullTerminated len = truncateAt 0 <$> getByteString len

getPath :: Get FilePath
getPath = getNullTerminated mAX_PATH

fLAG_WRITE :: Word32
fLAG_WRITE = 1
fLAG_CREATE :: Word32
fLAG_CREATE = 2

{-# INLINE parseOpen #-}
parseOpen :: Get Func
parseOpen = mkOpen <$> getPath <*> getWord32le <*> getWord32le
  where
    mkOpen path flags mode =
      Open path (openMode flags) (creationMode flags mode)
    openMode flags
      | 0 /= flags .&. fLAG_WRITE = OpenWriteMode
      | otherwise = OpenReadMode
    creationMode flags mode
      | 0 /= flags .&. fLAG_CREATE = Create mode
      | otherwise = NoCreate

execP :: FilePath -> FilePath -> FilePath -> FilePath -> IO Func
execP file cwd envPath confStrPath
  | "/" `BS8.isInfixOf` file = return $ ExecP (Just file) []
  | otherwise = search [] allPaths
  where
    split = BS8.split ':'
    allPaths =
      map ((</> file) . (cwd </>)) $ split envPath ++ split confStrPath
    search attempted [] = return $ ExecP Nothing attempted
    search attempted (path:paths) = do
      canExec <- fileAccess path False False True `catchDoesNotExist` return False
      if canExec
        then return $ ExecP (Just path) attempted
        else search (path:attempted) paths

funcs :: IntMap (String, Get (IO Func))
funcs =
  M.fromList
  [ (0x10000, ("open"    , return <$> parseOpen))
  , (0x10001, ("creat"   , return <$> (Creat <$> getPath <*> getWord32le)))
  , (0x10002, ("stat"    , return <$> (Stat <$> getPath)))
  , (0x10003, ("lstat"   , return <$> (LStat <$> getPath)))
  , (0x10004, ("opendir" , return <$> (OpenDir <$> getPath)))
  , (0x10005, ("access"  , return <$> (Access <$> getPath <*> getWord32le)))
  , (0x10006, ("truncate", return <$> (Truncate <$> getPath <*> getWord64le)))
  , (0x10007, ("unlink"  , return <$> (Unlink <$> getPath)))
  , (0x10008, ("rename"  , return <$> (Rename <$> getPath <*> getPath)))
  , (0x10009, ("chmod"   , return <$> (Chmod <$> getPath <*> getWord32le)))
  , (0x1000A, ("readlink", return <$> (ReadLink <$> getPath)))
  , (0x1000B, ("mknod"   , return <$> (MkNod <$> getPath <*> getWord32le <*> getWord64le)))
  , (0x1000C, ("mkdir"   , return <$> (MkDir <$> getPath <*> getWord32le)))
  , (0x1000D, ("rmdir"   , return <$> (RmDir <$> getPath)))
  , (0x1000E, ("symlink" , return <$> (SymLink <$> getPath <*> getPath)))
  , (0x1000F, ("link"    , return <$> (Link <$> getPath <*> getPath)))
  , (0x10010, ("chown"   , return <$> (Chown <$> getPath <*> getWord32le <*> getWord32le)))
  , (0x10011, ("exec"    , return <$> (Exec <$> getPath)))
  , (0x10012, ("execp"   , execP <$> getNullTerminated mAX_EXEC_FILE <*> getPath <*> getNullTerminated mAX_PATH_ENV_VAR_LENGTH <*> getNullTerminated mAX_PATH_CONF_STR))
  ]

{-# INLINE parseMsgLazy #-}
parseMsgLazy :: BSL.ByteString -> IO Func
parseMsgLazy = runGet $ do
  funcId <- getWord32le
  let (_name, getter) = funcs ! fromIntegral funcId
  func <- getter
  finished <- isEmpty
  unless finished $ fail "Unexpected trailing input in message"
  return func

{-# INLINE strictToLazy #-}
strictToLazy :: ByteString -> BSL.ByteString
strictToLazy x = BSL.fromChunks [x]

{-# INLINE parseMsg #-}
parseMsg :: ByteString -> IO Func
parseMsg = parseMsgLazy . strictToLazy
