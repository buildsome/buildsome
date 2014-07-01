module Buildsome.FileContentDescCache
  ( fileContentDescOfStat
  ) where

import Buildsome.Db (Db)
import Lib.FileDesc (FileContentDesc)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Buildsome.Db as Db
import qualified Lib.FileDesc as FileDesc
import qualified System.Posix.ByteString as Posix

fileContentDescOfStat :: Db -> FilePath -> Posix.FileStatus -> IO FileContentDesc
fileContentDescOfStat db path stat = do
  mDescCache <- Db.readIRef cacheIRef
  case mDescCache of
    Just oldCache
      | Posix.modificationTimeHiRes stat ==
        Db.fcdcModificationTime oldCache ->
        return $ Db.fcdcFileContentDesc oldCache
    _ -> do
      newFileContentDesc <- FileDesc.fileContentDescOfStat path stat
      Db.writeIRef cacheIRef Db.FileContentDescCache
        { Db.fcdcModificationTime = Posix.modificationTimeHiRes stat
        , Db.fcdcFileContentDesc = newFileContentDesc
        }
      return newFileContentDesc
  where
    cacheIRef = Db.fileContentDescCache path db
