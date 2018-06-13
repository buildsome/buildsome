module Lib.ScanFileUpwards (scanFileUpwards) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except
import           Data.Foldable (traverse_)
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath)

scanFileUpwards :: FilePath -> IO (Maybe FilePath)
scanFileUpwards name = do
  cwd <- Posix.getWorkingDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate FilePath.takeDirectory cwd
    candidates = map (</> name) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runExceptT $ traverse_ check candidates
  case res of
    Left found -> Just <$> FilePath.makeRelativeToCurrentDirectory found
    Right () -> return Nothing
  where
    check path = do
      exists <- liftIO $ FilePath.exists path
      when exists $ throwE path
