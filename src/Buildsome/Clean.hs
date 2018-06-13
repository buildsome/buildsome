module Buildsome.Clean (Result(..), output) where

import Prelude.Compat hiding (FilePath)

import Lib.Directory (getMFileStatus, removeFileOrDirectory)
import Lib.FilePath (FilePath)

import System.Posix.ByteString (FileOffset, fileSize)

data Result = Result
    { cleanedTotalSize :: FileOffset
    , cleanedTotalEstimatedSpace :: FileOffset
    , cleanedTotalCount :: Int
    }
instance Semigroup Result where
    Result asize aspace acount <> Result bsize bspace bcount =
        Result (asize + bsize) (aspace + bspace) (acount + bcount)
instance Monoid Result where
    mempty = Result 0 0 0

align :: Integral a => a -> a -> a
align alignment x = ((x + alignment - 1) `div` alignment) * alignment
{-# INLINE align #-}

output :: FilePath -> IO Result
output path = do
    mStat <- getMFileStatus path
    case mStat of
        Nothing -> pure mempty
        Just stat -> do
          removeFileOrDirectory path
          pure
            Result
            { cleanedTotalSize = fileSize stat
            , cleanedTotalEstimatedSpace = align 4096 (fileSize stat)
            , cleanedTotalCount = 1
            }
