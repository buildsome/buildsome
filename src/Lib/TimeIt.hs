module Lib.TimeIt
  ( timeIt
  ) where

import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)

timeIt :: IO a -> IO (NominalDiffTime, a)
timeIt act = do
  before <- getCurrentTime
  res <- act
  after <- getCurrentTime
  return (after `diffUTCTime` before, res)
