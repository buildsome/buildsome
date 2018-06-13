{-# LANGUAGE DeriveDataTypeable #-}
module Buildsome.Meddling
  ( ThirdPartyMeddlingError(..), assertFileMTime, assertSameMTime
  ) where

import Prelude.Compat hiding (FilePath)

import Control.Monad
import Data.Typeable (Typeable)
import Lib.FileDesc (fileStatDescOfStat)
import Lib.FilePath (FilePath)
import qualified Control.Exception as E
import qualified Lib.Directory as Dir
import qualified System.Posix.ByteString as Posix

data ThirdPartyMeddlingError = ThirdPartyMeddlingError FilePath String deriving (Show, Typeable)
instance E.Exception ThirdPartyMeddlingError

-- TODO: Rename MTime to something else, it tests more than mtime
assertFileMTime :: String -> FilePath -> Maybe Posix.FileStatus -> IO ()
assertFileMTime msgPrefix path oldMStat = do
  newMStat <- Dir.getMFileStatus path
  assertSameMTime msgPrefix path oldMStat newMStat

assertSameMTime ::
  String -> FilePath -> Maybe Posix.FileStatus -> Maybe Posix.FileStatus -> IO ()
assertSameMTime msgPrefix path oldMStat newMStat =
  case (oldMStat, newMStat) of
  (Nothing, Just _) -> fileErr "created"
  (Just _, Nothing) -> fileErr "deleted"
  (Nothing, Nothing) -> pure ()
  (Just oldStat, Just newStat)
    | Posix.isDirectory newStat ->
      unless
        (fileStatDescOfStat oldStat ==
         fileStatDescOfStat newStat) $ err "Directory modified"
    | Posix.modificationTimeHiRes oldStat /=
      Posix.modificationTimeHiRes newStat -> fileErr "changed"
    | otherwise -> pure ()
  where
    err msg = E.throwIO $ ThirdPartyMeddlingError path (msgPrefix ++ ": " ++ msg)
    fileErr verb = err $ "File " ++ verb ++ " during build!"
