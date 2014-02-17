{-# OPTIONS -Wall -O2 #-}
module Lib.Sock
  ( recvLoop
  , unixSeqPacketListener
  ) where

import Network.Socket (Socket, socket)
import qualified Data.ByteString as BS
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS

recvLoop :: Int -> Socket -> IO [BS.ByteString]
recvLoop maxFrameSize sock = do
  frame <- SockBS.recv sock maxFrameSize
  if BS.null frame then return []
    else do
      rest <- recvLoop maxFrameSize sock
      return (frame : rest)

unixSeqPacketListener :: FilePath -> IO Socket
unixSeqPacketListener path = do
  serverSock <- socket Sock.AF_UNIX Sock.SeqPacket 0
  Sock.bind serverSock (Sock.SockAddrUnix path)
  Sock.listen serverSock 5
  return serverSock
