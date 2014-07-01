{-# LANGUAGE DeriveDataTypeable #-}
module Buildsome.Meddling
  ( ThirdPartyMeddlingError(..), assertSameMTime
  ) where

import Control.Monad
import Data.Typeable (Typeable)
import Lib.FileDesc (fileStatDescOfStat)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Control.Exception as E
import qualified Lib.Directory as Dir
import qualified System.Posix.ByteString as Posix

data ThirdPartyMeddlingError = ThirdPartyMeddlingError FilePath String deriving (Show, Typeable)
instance E.Exception ThirdPartyMeddlingError

assertSameMTime :: FilePath -> Maybe Posix.FileStatus -> IO ()
assertSameMTime path oldMStat = do
  newMStat <- Dir.getMFileStatus path
  case (oldMStat, newMStat) of
    (Nothing, Just _) -> fileErr "created"
    (Just _, Nothing) -> fileErr "deleted"
    (Nothing, Nothing) -> return ()
    (Just oldStat, Just newStat)
      | Posix.isDirectory newStat ->
        unless
          (fileStatDescOfStat oldStat ==
           fileStatDescOfStat newStat) $ err "Directory modified"
      | Posix.modificationTimeHiRes oldStat /=
        Posix.modificationTimeHiRes newStat -> fileErr "changed"
      | otherwise -> return ()
  where
    err = E.throwIO . ThirdPartyMeddlingError path
    fileErr verb = err $ "File " ++ verb ++ " during build!"
