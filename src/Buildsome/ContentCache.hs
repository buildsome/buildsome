{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Buildsome.ContentCache where

import           Buildsome.Types (Buildsome(..), BuildTargetEnv(..))
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength)
import qualified Data.ByteString.Base16 as Base16
import qualified Buildsome.Color as Color
import           Control.Monad (unless, when, forM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Either (EitherT(..), left)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String (IsString(..))
import           Lib.Directory (getMFileStatus, removeFileOrDirectoryOrNothing)
import qualified Lib.Directory as Dir
import           Lib.FileDesc (FileContentDesc(..), BasicStatEssence(..), FullStatEssence(..), FileStatDesc(..))
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import           Lib.Printer (printStrLn)
import           Lib.Hash (Hash)
import qualified Lib.Hash as Hash
import           Lib.Show (show)
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath, show)


const_CACHED_OUTPUTS_DIR :: FilePath
const_CACHED_OUTPUTS_DIR = "cached_outputs"
const_MAX_CACHE_FILE_AGE :: POSIXTime
const_MAX_CACHE_FILE_AGE = posixDayLength * 5


contentCacheDir :: Buildsome -> FilePath
contentCacheDir buildsome = bsBuildsomePath buildsome </> const_CACHED_OUTPUTS_DIR

cleanContentCacheDir :: Buildsome -> IO ()
cleanContentCacheDir buildsome = do
  currentTime <- getPOSIXTime
  files <- Dir.getDirectoryContents $ contentCacheDir buildsome
  removed <- forM files $ \fileName -> do
    mFileStatus <- getMFileStatus fileName
    case mFileStatus of
      Nothing -> return Nothing
      Just fileStatus ->
        if Posix.modificationTimeHiRes fileStatus - currentTime > const_MAX_CACHE_FILE_AGE
        then do
          removeFileOrDirectoryOrNothing fileName
          return $ Just fileName
        else
          return Nothing
  let numRemoved = length $ catMaybes removed
  putStrLn $ concat
    [ "Cache contains ", show (length removed)
    , ", removed ", show numRemoved, " old cache files" ]

mkTargetWithHashPath :: Buildsome -> Hash -> FilePath
mkTargetWithHashPath buildsome contentHash = contentCacheDir buildsome </> Base16.encode (Hash.asByteString contentHash)-- (outPath <> "." <> Base16.encode contentHash)

refreshFromContentCache :: (IsString e, MonadIO m) =>
  BuildTargetEnv -> FilePath -> Maybe FileContentDesc -> Maybe FileStatDesc -> EitherT e m ()
refreshFromContentCache
  BuildTargetEnv{..} filePath (Just (FileContentDescRegular contentHash)) (Just (FileStatOther fullStat)) = do
  liftIO (FilePath.exists cachedPath) >>= \oldExists ->
    unless oldExists $ left "No cached copy"
  liftIO $ do
    printStrLn btePrinter $ bsRender bteBuildsome
      $ mconcat [ "Copying: " <> cPath (show cachedPath) <> " -> " <> cPath (show filePath) ]
    removeIfExists filePath
    removeIfExists tempFile
    Dir.createDirectories $ FilePath.takeDirectory filePath
    -- Set stat attributes before creating the file, so the target file is created with correct
    -- attrs from the start
    -- TODO use proper tempfile naming
    Dir.copyFile cachedPath  tempFile
    -- TODO set other stat fields?
    -- TODO these may cause failure later (over permissions) if the user running now is not the one
    -- recorded in the log!
    Posix.setFileMode tempFile (fileMode $ basicStatEssence fullStat)
    Posix.setOwnerAndGroup tempFile (fileOwner $ basicStatEssence fullStat) (fileGroup $ basicStatEssence fullStat)
    Dir.renameFile tempFile filePath
    Posix.setFileTimesHiRes filePath (statusChangeTimeHiRes fullStat) (modificationTimeHiRes fullStat)

  where Color.Scheme{..} = Color.scheme
        cachedPath = mkTargetWithHashPath bteBuildsome contentHash
        tempFile = filePath <> "._buildsome_temp"
        removeIfExists f =
          FilePath.exists f >>= \exists -> when exists $ removeFileOrDirectoryOrNothing f

refreshFromContentCache _ _ _ _ = left "No cached info"

