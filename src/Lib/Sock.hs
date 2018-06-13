module Lib.Sock
  ( recvAll, recvFrame, recvLoop_
  , withUnixStreamListener
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Word (Word32)
import           Lib.Binary (decode)
import           Lib.Exception (bracket, bracket_)
import           Lib.FilePath (FilePath)
import           Network.Socket (Socket)
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath)

-- May not receive all if EOF encountered
recvAll :: Socket -> Int -> IO BS.ByteString
recvAll _ n | n < 0 = error "recvAll: negative length"
recvAll _ 0 = pure mempty
recvAll sock n = do
  dat <- SockBS.recv sock n
  if BS.null dat then pure dat
  else (dat <>) <$> recvAll sock (n - BS.length dat)

recvFrame :: Socket -> IO (Maybe BS.ByteString)
recvFrame sock = do
  frameSizeStr <- recvAll sock 4
  if BS.null frameSizeStr
    then pure Nothing
    else Just <$> recvAll sock (fromIntegral (decode frameSizeStr :: Word32))

{-# INLINE recvLoop_ #-}
recvLoop_ :: (BS.ByteString -> IO ()) -> Socket -> IO ()
recvLoop_ f sock = go
  where
    go = do
      mFrame <- recvFrame sock
      case mFrame of
        Nothing -> pure ()
        Just frame -> do
          f frame
          go

withUnixStreamListener :: FilePath -> (Socket -> IO a) -> IO a
withUnixStreamListener path body =
  bracket (Sock.socket Sock.AF_UNIX Sock.Stream 0) Sock.close $ \sock ->
  bracket_ (Sock.bind sock (Sock.SockAddrUnix (BS8.unpack path))) (Posix.removeLink path) $ do
    Sock.listen sock 5
    body sock
