module Buildsome.FileContentDescCache
  ( fileContentDescOfStat
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.Db (Db)
import Lib.FileDesc (FileContentDesc)
import Lib.FilePath (FilePath)
import qualified Buildsome.Db as Db
import qualified Buildsome.Meddling as Meddling
import qualified Lib.FileDesc as FileDesc
import qualified System.Posix.ByteString as Posix

fileContentDescOfStat :: String -> Db -> FilePath -> Posix.FileStatus -> IO FileContentDesc
fileContentDescOfStat msgPrefix db path stat = do
  mDescCache <- Db.readIRef cacheIRef
  case mDescCache of
    Just oldCache
      | Posix.modificationTimeHiRes stat ==
        Db.fcdcModificationTime oldCache ->
        pure $ Db.fcdcFileContentDesc oldCache
    _ -> do
      newFileContentDesc <- FileDesc.fileContentDescOfStat path stat

      -- TODO: May be more optimal to delay the writeIRef until later
      -- when we check the stat again once
      Meddling.assertFileMTime msgPrefix path $ Just stat

      Db.writeIRef cacheIRef Db.FileContentDescCache
        { Db.fcdcModificationTime = Posix.modificationTimeHiRes stat
        , Db.fcdcFileContentDesc = newFileContentDesc
        }
      pure newFileContentDesc
  where
    cacheIRef = Db.fileContentDescCache path db
