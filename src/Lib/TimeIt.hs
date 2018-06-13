module Lib.TimeIt
    ( timeIt
    , printTimeIt
    , pureTimeIt
    ) where

import Control.Exception (evaluate)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.IO.Unsafe

import Prelude.Compat

timeIt :: IO a -> IO (NominalDiffTime, a)
timeIt act =
    do
        before <- getCurrentTime
        res <- act
        after <- getCurrentTime
        return (after `diffUTCTime` before, res)

printTimeIt :: String -> IO a -> IO a
printTimeIt msg act =
    do
        (t, res) <- timeIt act
        putStrLn $ msg ++ " took " ++ show t
        return res

pureTimeIt :: String -> a -> a
pureTimeIt msg x = unsafePerformIO $ printTimeIt msg (evaluate x)
