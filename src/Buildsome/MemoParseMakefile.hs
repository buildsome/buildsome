{-# LANGUAGE NoImplicitPrelude #-}
module Buildsome.MemoParseMakefile
  ( IsHit(..), memoParse
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.Db (Db)
import Buildsome.FileContentDescCache (fileContentDescOfStat)

import Control.DeepSeq (rnf)
import Data.Map (Map)
import Lib.FileDesc (FileContentDesc)
import Lib.FilePath (FilePath)
import Lib.Makefile (Makefile, Vars)
import qualified Buildsome.Db as Db
import qualified Buildsome.Meddling as Meddling
import qualified Control.Exception as E
import qualified Data.Map as Map
import qualified Lib.Directory as Dir
import qualified Lib.Makefile as Makefile
import qualified Lib.Makefile.Monad as MakefileMonad
import qualified System.Posix.ByteString as Posix

mkFileDesc :: Db -> FilePath -> Maybe Posix.FileStatus -> IO (Db.FileDesc () FileContentDesc)
mkFileDesc _ _ Nothing = return $ Db.FileDescNonExisting ()
mkFileDesc db path (Just stat) =
  Db.FileDescExisting <$>
  fileContentDescOfStat "When parsing Makefile" db path stat

parse :: Db -> FilePath -> Vars -> IO (Map FilePath (Db.FileDesc () FileContentDesc), [MakefileMonad.PutStrLn], Makefile)
parse db absMakefilePath vars = do
  (readFiles, putStrLns, res) <-
    MakefileMonad.runM $
    Makefile.parse absMakefilePath vars
  contentDescs <- Map.traverseWithKey (mkFileDesc db) readFiles
  -- This must come after:

  -- TODO: This seems redundant, but is it? The mkFileDesc only
  -- asserts mtimes if it's not from cache
  mapM_ (uncurry (Meddling.assertFileMTime "When parsing Makefile")) $
    Map.toList readFiles
  return (contentDescs, putStrLns, res)

cacheInputsMatch ::
  Db ->
  (FilePath, Map FilePath Db.MFileContentDesc) ->
  FilePath -> IO Bool
cacheInputsMatch db (oldPath, oldFiles) path
  | oldPath /= path = return False
  | otherwise =
    fmap and $ mapM verifySameContent $ Map.toList oldFiles
  where
    verifySameContent (filePath, oldContentDesc) = do
      mStat <- Dir.getMFileStatus filePath
      newContentDesc <- mkFileDesc db filePath mStat
      return $ oldContentDesc == newContentDesc

data IsHit = Hit | Miss

matchCache ::
  Db -> FilePath -> Maybe Db.MakefileParseCache ->
  ((Makefile, [MakefileMonad.PutStrLn]) -> IO r) -> IO r -> IO r
matchCache _ _ Nothing _ miss = miss
matchCache db absMakefilePath (Just (Db.MakefileParseCache inputs output)) hit miss = do
  isMatch <- cacheInputsMatch db inputs absMakefilePath
  if isMatch
    then hit output
    else miss

memoParse :: Db -> FilePath -> Vars -> IO (IsHit, Makefile)
memoParse db absMakefilePath vars = do
  mCache <- Db.readIRef makefileParseCacheIRef
  (isHit, makefile) <-
    matchCache db absMakefilePath mCache hit $ do
      (newFiles, putStrLns, makefile) <- parse db absMakefilePath vars
      Db.writeIRef makefileParseCacheIRef $
        Db.MakefileParseCache (absMakefilePath, newFiles) (makefile, putStrLns)
      return (Miss, makefile)
  E.evaluate $ rnf makefile
  return (isHit, makefile)
  where
    hit (makefile, putStrLns) = do
      mapM_ MakefileMonad.runPutStrLn putStrLns
      return (Hit, makefile)
    makefileParseCacheIRef = Db.makefileParseCache db vars
