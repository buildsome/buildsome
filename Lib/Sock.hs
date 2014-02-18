{-# OPTIONS -Wall -O2 #-}
module Lib.Sock
  ( recvLoop_
  , unixSeqPacketListener
  ) where

import Control.Monad (when)
import Network.Socket (Socket, socket)
import qualified Data.ByteString as BS
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

recvLoop_ :: Int -> (BS.ByteString -> IO ()) -> Socket -> IO ()
recvLoop_ maxFrameSize f sock = do
  frame <- SockBS.recv sock maxFrameSize
  when (not (BS.null frame)) $ do
    f frame
    recvLoop_ maxFrameSize f sock

unixSeqPacketListener :: FilePath -> IO Socket
unixSeqPacketListener path = do
  serverSock <- socket Sock.AF_UNIX Sock.SeqPacket 0
  Sock.bind serverSock (Sock.SockAddrUnix path)
  Sock.listen serverSock 5
  return serverSock
