{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Lib.Makefile.Monad
  ( PutStrLn, runPutStrLn
  , M, runM
  ) where


import Prelude.Compat hiding (FilePath)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)

import GHC.Generics (Generic)
import Lib.Directory (getMFileStatus)
import Lib.FilePath (FilePath)
import Lib.Makefile.MonadClass (MonadMakefileParser(..))
import qualified Buildsome.Meddling as Meddling
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

-- Specific Monad type for makefile parsing that tracks all inputs/outputs:
data PutStrLnTarget = Out | Err deriving (Generic, Show)
data PutStrLn = PutStrLn PutStrLnTarget ByteString deriving (Generic, Show)
instance Binary PutStrLnTarget
instance Binary PutStrLn

targetHandle :: PutStrLnTarget -> IO.Handle
targetHandle Out = IO.stdout
targetHandle Err = IO.stderr

type ReadFiles = Map FilePath (Maybe Posix.FileStatus)

data W = W
  { wPutStrLns :: [PutStrLn]
  , wReadFiles :: Map FilePath [Maybe Posix.FileStatus]
  }
instance Monoid W where
  mempty = W mempty mempty
  mappend (W ax ay) (W bx by) =
    W (mappend ax bx) (Map.unionWith (++) ay by)

runPutStrLn :: PutStrLn -> IO ()
runPutStrLn (PutStrLn target bs) = BS8.hPutStrLn (targetHandle target) bs

-- IMPORTANT: Use StateT and not WriterT, because WriterT's >>= leaks
-- stack-space due to tuple unpacking (and mappend) *after* the inner
-- bind, whereas StateT uses a tail call for the inner bind.
newtype M a = M (StateT W IO a)
  deriving (Functor, Applicative, Monad)
tell :: W -> M ()
tell = M . State.modify . flip mappend

doPutStrLn :: PutStrLnTarget -> ByteString -> M ()
doPutStrLn tgt bs = do
  M $ liftIO $ runPutStrLn p
  tell $ mempty { wPutStrLns = [p] }
  where
    p = PutStrLn tgt bs

instance MonadMakefileParser M where
  outPutStrLn = doPutStrLn Out
  errPutStrLn = doPutStrLn Err
  tryReadFile filePath = do
    mFileStat <- M $ liftIO $ getMFileStatus filePath
    tell $ mempty { wReadFiles = Map.singleton filePath [mFileStat] }
    M $ liftIO $ tryReadFile filePath

mergeAllFileStatuses :: FilePath -> [Maybe Posix.FileStatus] -> IO (Maybe Posix.FileStatus)
mergeAllFileStatuses filePath = go
  where
    go [] = fail "Empty list impossible"
    go [x] = return x
    go (x:xs) = do
      r <- go xs
      Meddling.assertSameMTime "When checking Makefile inputs" filePath x r
      return x

runM :: M a -> IO (ReadFiles, [PutStrLn], a)
runM (M act) = do
  (res, w) <- runStateT act mempty
  readFiles <- Map.traverseWithKey mergeAllFileStatuses (wReadFiles w)
  return (readFiles, wPutStrLns w, res)
