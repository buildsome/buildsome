module FileDescCache
  ( getFileDesc, fileDescOfMStat
  ) where

import Db (Db)
import Lib.Directory (getMFileStatus)
import Lib.FileDesc (FileDesc)
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Db
import qualified System.Posix.ByteString as Posix
import qualified Lib.FileDesc as FileDesc

fileDescOfMStat :: Db -> FilePath -> Maybe Posix.FileStatus -> IO FileDesc
fileDescOfMStat db path oldMStat = do
  mDescCache <- Db.readIRef cacheIRef
  let getNewDesc = FileDesc.fileDescOfMStat path oldMStat
  case (mDescCache, oldMStat) of
    (Nothing, Nothing) -> getNewDesc
    (Just _, Nothing) -> do
      Db.delIRef cacheIRef
      getNewDesc
    (Just oldCache, Just stat)
      | Posix.modificationTime stat ==
        Db.fdcModificationTime oldCache -> do
        newMStat <- getMFileStatus path
        FileDesc.assertSameMTimes path oldMStat newMStat
        return $ Db.fdcFileDesc oldCache
    (_, Just stat) -> do
      newFileDesc <- getNewDesc
      Db.writeIRef cacheIRef Db.FileDescCache
        { Db.fdcModificationTime = Posix.modificationTime stat
        , Db.fdcFileDesc = newFileDesc
        }
      return newFileDesc
  where
    cacheIRef = Db.fileDescCache path db

getFileDesc :: Db -> FilePath -> IO FileDesc
getFileDesc db path = fileDescOfMStat db path =<< getMFileStatus path
