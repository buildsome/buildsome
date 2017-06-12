{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Lib.FSHook.Protocol
  ( parseMsg, helloPrefix
  , OpenWriteMode(..), showOpenWriteMode
  , OpenTruncateMode(..), showOpenTruncateMode
  , CreationMode(..), showCreationMode
  , InFilePath, OutFilePath(..), OutEffect(..)
  , Severity(..)
  , Func(..), showFunc
  , IsDelayed(..)
  , Msg(..)
  ) where

import Control.Monad
import Data.Binary.Get
import Data.Binary (Binary(..))
import GHC.Generics (Generic(..))
import Data.Bits
import Data.ByteString (ByteString)
import Data.IntMap (IntMap, (!))
import Data.Word
import Lib.ByteString (truncateAt)
import Lib.Directory (catchDoesNotExist)
import Lib.FilePath (FilePath, (</>))
import Numeric (showOct)
import System.Posix.Files.ByteString (fileAccess)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M
import Hexdump (prettyHex)

import Prelude.Compat hiding (FilePath)

data OpenWriteMode = WriteMode | ReadWriteMode
  deriving (Show, Generic)
instance Binary OpenWriteMode
showOpenWriteMode :: OpenWriteMode -> String
showOpenWriteMode WriteMode = ""
showOpenWriteMode ReadWriteMode = "+"
{-# INLINE showOpenWriteMode #-}

data CreationMode = NoCreate | Create Word32 -- Unix permissions
  deriving (Show, Generic)
instance Binary CreationMode
showCreationMode :: CreationMode -> String
showCreationMode NoCreate = ""
showCreationMode (Create x) = " (CREATE:" ++ showOct x "" ++ ")"
{-# INLINE showCreationMode #-}

data OpenTruncateMode = OpenNoTruncate | OpenTruncate
  deriving (Show, Generic)
instance Binary OpenTruncateMode
showOpenTruncateMode :: OpenTruncateMode -> String
showOpenTruncateMode OpenNoTruncate = ""
showOpenTruncateMode OpenTruncate = " (TRUNCATE)"

type InFilePath = FilePath

-- WARNING: The order of these constructors must match
-- fs_override.c:enum out_effect (due to Enum instance)!
data OutEffect
  = OutEffectNothing
  | OutEffectCreated
  | OutEffectDeleted
  | OutEffectChanged
  | OutEffectUnknown
  deriving (Eq, Ord, Show, Enum, Generic)
instance Binary OutEffect

data Severity
  = SeverityDebug
  | SeverityWarning
  | SeverityError
  deriving (Eq, Ord, Show, Enum, Generic)
instance Binary Severity

data OutFilePath = OutFilePath
  { outPath :: FilePath
  , outEffect :: OutEffect
  } deriving (Eq, Ord, Show)

data Func
  = OpenR InFilePath
  | OpenW OpenWriteMode CreationMode OpenTruncateMode OutFilePath
  | Stat InFilePath
  | LStat InFilePath
  | Creat Word32 OutFilePath
  | Rename OutFilePath OutFilePath
  | Unlink Word32 OutFilePath
  | Access Word32 InFilePath{- TODO: replace Int with AccessMode -}
  | OpenDir InFilePath
  | Truncate Word64 OutFilePath{- length -}
  | Chmod Word32 OutFilePath{-mode-}
  | ReadLink InFilePath
  | MkNod Word32{-mode-} Word64{-dev-} OutFilePath
  | MkDir Word32{-mode-} OutFilePath
  | RmDir OutFilePath
  | SymLink InFilePath OutFilePath
  | Link OutFilePath OutFilePath
  | Chown Word32 Word32 OutFilePath
  | Exec InFilePath
  | ExecP (Maybe FilePath) [FilePath]{-prior searched paths (that did not exist)-}
  | RealPath InFilePath
  | Trace Severity ByteString
  deriving Show

-- Hook is delayed waiting for handler to complete
data IsDelayed = Delayed | NotDelayed

data Msg = Msg
  { msgIsDelayed :: IsDelayed
  , msgFunc :: Func
  }

{-# INLINE showFunc #-}
showFunc :: Func -> String
showFunc (OpenR path) = "open:" ++ show path
showFunc (OpenW wmode creation trunc path) = "openW" ++ showOpenWriteMode wmode ++ ":" ++ show path ++ showCreationMode creation ++ showOpenTruncateMode trunc
showFunc (Stat path) = "stat:" ++ show path
showFunc (LStat path) = "lstat:" ++ show path
showFunc (Creat perms path) = concat ["create:", show path, " ", showOct perms ""]
showFunc (Rename old new) = concat ["rename:", show old, "->", show new]
showFunc (Unlink mode path) = concat ["unlink:", show path, " ", show mode]
showFunc (Access mode path) = concat ["access:", show path, " ", show mode]
showFunc (OpenDir path) = concat ["openDir:", show path]
showFunc (Truncate len path) = concat ["truncate:", show path, " ", show len]
showFunc (Chmod perms path) = concat ["chmod:", show path, " ", showOct perms ""]
showFunc (ReadLink path) = concat ["readlink:", show path]
showFunc (MkNod mode dev path) = unwords ["mknod:", show path, showOct mode "", show dev]
showFunc (MkDir mode path) = unwords ["mkdir:", show path, showOct mode ""]
showFunc (RmDir path) = unwords ["rmdir:", show path]
showFunc (SymLink target linkpath) = unwords ["symlink:", show target, show linkpath]
showFunc (Link src dest) = unwords ["link:", show src, show dest]
showFunc (Chown uid gid path) = unwords ["chown:", show path, show uid, show gid]
showFunc (Exec path) = unwords ["exec:", show path]
showFunc (ExecP (Just path) attempted) = unwords ["execP:", show path, "searched:", show attempted]
showFunc (ExecP Nothing attempted) = unwords ["failedExecP:searched:", show attempted]
showFunc (RealPath path) = unwords ["realPath:", show path]
showFunc (Trace severity msg) = unwords ["trace:", show severity, BS8.unpack msg]

{-# ANN module ("HLint: ignore Use ++"::String) #-}
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

mAX_PATH :: Int
mAX_PATH = 1024
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

getPathEOM :: Get FilePath
getPathEOM = do
  lb <- getLazyByteStringNul
  return $ BS8.concat $ BSL.toChunks lb

getInPath :: Get InFilePath
getInPath = getPath

getInPathEOM :: Get InFilePath
getInPathEOM = getPathEOM

getOutEffect :: Get OutEffect
getOutEffect = toEnum . fromIntegral <$> getWord32le

getOutPath :: Get OutFilePath
getOutPath = flip OutFilePath <$> getOutEffect <*> getPath

getOutPathEOM :: Get OutFilePath
getOutPathEOM = flip OutFilePath <$> getOutEffect <*> getPathEOM

getSeverity :: Get Severity
getSeverity = toEnum . fromIntegral <$> getWord32le

fLAG_ALSO_READ :: Word32
fLAG_ALSO_READ = 1
fLAG_CREATE :: Word32
fLAG_CREATE = 2
fLAG_TRUNCATE :: Word32
fLAG_TRUNCATE = 4

{-# INLINE parseOpenW #-}
parseOpenW :: Get Func
parseOpenW = mkOpen <$> getWord32le <*> getWord32le <*> getOutPathEOM
  where
    mkOpen flags mode path =
      OpenW (openMode flags) (creationMode flags mode) (isTruncate flags) path
    openMode flags
      | 0 /= flags .&. fLAG_ALSO_READ = ReadWriteMode
      | otherwise = WriteMode
    creationMode flags mode
      | 0 /= flags .&. fLAG_CREATE = Create mode
      | otherwise = NoCreate
    isTruncate flags
      | 0 /= flags .&. fLAG_TRUNCATE = OpenTruncate
      | otherwise = OpenNoTruncate

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
  [ (0x10000, ("openR"   , return <$> (OpenR <$> getInPathEOM)))
  , (0x10001, ("openW"   , return <$> parseOpenW)) -- TODO: Parse here, do post-process in func like execP
  , (0x10002, ("creat"   , return <$> (Creat <$> getWord32le <*> getOutPathEOM)))
  , (0x10003, ("stat"    , return <$> (Stat <$> getInPathEOM)))
  , (0x10004, ("lstat"   , return <$> (LStat <$> getInPathEOM)))
  , (0x10005, ("opendir" , return <$> (OpenDir <$> getInPathEOM)))
  , (0x10006, ("access"  , return <$> (Access <$> getWord32le <*> getInPathEOM)))
  , (0x10007, ("truncate", return <$> (Truncate <$> getWord64le <*> getOutPathEOM)))
  , (0x10008, ("unlink"  , return <$> (Unlink <$> getWord32le <*> getOutPathEOM)))
  , (0x10009, ("rename"  , return <$> (Rename <$> getOutPath <*> getOutPathEOM)))
  , (0x1000A, ("chmod"   , return <$> (Chmod <$> getWord32le <*> getOutPathEOM)))
  , (0x1000B, ("readlink", return <$> (ReadLink <$> getInPathEOM)))
  , (0x1000C, ("mknod"   , return <$> (MkNod <$> getWord32le <*> getWord64le <*> getOutPathEOM)))
  , (0x1000D, ("mkdir"   , return <$> (MkDir <$> getWord32le <*> getOutPathEOM)))
  , (0x1000E, ("rmdir"   , return <$> (RmDir <$> getOutPathEOM)))
  , (0x1000F, ("symlink" , return <$> (SymLink <$> getInPath <*> getOutPathEOM)))
  , (0x10010, ("link"    , return <$> (Link <$> getOutPath <*> getOutPathEOM)))
  , (0x10011, ("chown"   , return <$> (Chown <$> getWord32le <*> getWord32le <*> getOutPathEOM)))
  , (0x10012, ("exec"    , return <$> (Exec <$> getInPathEOM)))
  , (0x10013, ("execp"   , execP <$> getNullTerminated mAX_EXEC_FILE <*> getPath <*> getNullTerminated mAX_PATH_ENV_VAR_LENGTH <*> getNullTerminated mAX_PATH_CONF_STR))
  , (0x10014, ("realPath", return <$> (RealPath <$> getInPathEOM)))
  , (0xF0000, ("trace"   , return <$> (Trace <$> getSeverity <*> getNullTerminated 1024)))
  ]

{-# INLINE parseMsgLazy #-}
parseMsgLazy :: BSL.ByteString -> IO Msg
parseMsgLazy bs =
  Msg isDelayed <$> mkFunc
  where
    (isDelayed, mkFunc) = (`runGet` bs) $ do
      isDelayedInt <- getWord8
      funcId <- getWord32le
      let (_name, getter) = funcs ! fromIntegral funcId
      ioFunc <- getter
      finished <- isEmpty
      unless finished $ do
        fail $ "Unexpected trailing input in message: "
           ++ (prettyHex $ BS8.concat $ BSL.toChunks bs)
      return (if isDelayedInt == 0 then NotDelayed else Delayed, ioFunc)

{-# INLINE strictToLazy #-}
strictToLazy :: ByteString -> BSL.ByteString
strictToLazy x = BSL.fromChunks [x]

{-# INLINE parseMsg #-}
parseMsg :: ByteString -> IO Msg
parseMsg = parseMsgLazy . strictToLazy

{-# INLINE helloPrefix #-}
helloPrefix :: ByteString
helloPrefix = "PROTOCOL10: HELLO, I AM: "
