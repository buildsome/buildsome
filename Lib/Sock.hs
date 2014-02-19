{-# OPTIONS -Wall -O2 #-}
module Lib.Sock
  ( recvLoop_
  , withUnixSeqPacketListener
  ) where

import Control.Monad (when)
import Network.Socket (Socket)
import System.Directory (removeFile)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

recvLoop_ :: Int -> (BS.ByteString -> IO ()) -> Socket -> IO ()
recvLoop_ maxFrameSize f sock = do
  frame <- SockBS.recv sock maxFrameSize
  when (not (BS.null frame)) $ do
    f frame
    recvLoop_ maxFrameSize f sock

withUnixSeqPacketListener :: FilePath -> (Socket -> IO a) -> IO a
withUnixSeqPacketListener path body =
  E.bracket (Sock.socket Sock.AF_UNIX Sock.SeqPacket 0) Sock.close $ \sock ->
  E.bracket_ (Sock.bind sock (Sock.SockAddrUnix path)) (removeFile path) $ do
    Sock.listen sock 5
    body sock
