module Lib.Sock
  ( recvLoop_
  , withUnixSeqPacketListener
  ) where

import Control.Monad (unless)
import Lib.FilePath (FilePath)
import Network.Socket (Socket)
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified System.Posix.ByteString as Posix

recvLoop_ :: Int -> (BS.ByteString -> IO ()) -> Socket -> IO ()
recvLoop_ maxFrameSize f sock = do
  frame <- SockBS.recv sock maxFrameSize
  unless (BS.null frame) $ do
    f frame
    recvLoop_ maxFrameSize f sock

withUnixSeqPacketListener :: FilePath -> (Socket -> IO a) -> IO a
withUnixSeqPacketListener path body =
  E.bracket (Sock.socket Sock.AF_UNIX Sock.SeqPacket 0) Sock.close $ \sock ->
  E.bracket_ (Sock.bind sock (Sock.SockAddrUnix (BS8.unpack path))) (Posix.removeLink path) $ do
    Sock.listen sock 5
    body sock
