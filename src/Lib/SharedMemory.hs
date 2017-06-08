{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.SharedMemory
  ( SharedMemory
  , newSharedMemory
  , sharedMemorySendFD
  , sharedMemoryAddFile
  ) where

import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Unsafe     as CU
import           Foreign.Ptr (Ptr)
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)

C.context (C.baseCtx <> C.bsCtx)
C.include "../../cbits/shared.h"

type SharedMemory = Ptr ()

newSharedMemory :: IO SharedMemory
newSharedMemory = [CU.block| void *{ return new_shmem(); } |]

sharedMemorySendFD :: SharedMemory -> C.CInt -> IO ()
sharedMemorySendFD sm fd = [CU.block| void {
    shmem_send_fd((shmem_context *)$(void *sm), $(int fd));
  } |]

sharedMemoryAddFile :: SharedMemory -> ByteString -> IO ()
sharedMemoryAddFile sm string = [CU.block| void {
  shmem_add_item_bs((shmem_context *)$(void *sm),
                      $bs-ptr:string, $bs-len:string
                      );
  } |]
