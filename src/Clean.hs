module Clean (Result(..), output) where

import Data.Monoid
import Lib.Directory (getMFileStatus, removeFileAllowNotExists)
import System.Posix.Types (FileOffset)
import System.Posix.Files (fileSize)

data Result = Result
  { cleanedTotalSize :: FileOffset
  , cleanedTotalEstimatedSpace :: FileOffset
  , cleanedTotalCount :: Int
  }
instance Monoid Result where
  mempty = Result 0 0 0
  mappend (Result asize aspace acount)
          (Result bsize bspace bcount) =
          (Result (asize+bsize) (aspace+bspace) (acount+bcount))

align :: Integral a => a -> a -> a
align alignment x = ((x + alignment - 1) `div` alignment) * alignment
{-# INLINE align #-}

output :: FilePath -> IO Result
output path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> return mempty
    Just stat -> do
      removeFileAllowNotExists path
      return
        Result
        { cleanedTotalSize = fileSize stat
        , cleanedTotalEstimatedSpace = align 4096 (fileSize stat)
        , cleanedTotalCount = 1
        }
