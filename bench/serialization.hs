{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Prelude.Compat          hiding (FilePath)

import           Buildsome.BuildId       (BuildId (..))
import qualified Buildsome.Db            as Db
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.ByteString.Lazy    (ByteString)
import qualified Lib.Directory           as Dir
import           Lib.FileDesc            (FileDesc (..), fileContentDescOfStat,
                                          fileModeDescOfStat,
                                          fileStatDescOfStat)
import           Lib.FilePath            (FilePath)
import           Lib.Hash
import           Lib.StdOutputs          (StdOutputs (..))
import qualified System.Posix.ByteString as Posix

import           Data.Binary

import           Criterion.Main

getFileDescInput :: MonadIO m => Db.Reason -> FilePath -> m Db.InputDesc
getFileDescInput reason filePath = {-# SCC "getFileDescInput" #-} do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case mStat of
      Nothing -> return $ Db.InputDescOfNonExisting reason
      Just stat -> do
          contentDesc <- liftIO $ fileContentDescOfStat filePath stat
          let time = Posix.modificationTimeHiRes stat
          return
              $ Db.InputDescOfExisting $ Db.ExistingInputDescOf
              { Db.idModeAccess = Just (reason, fileModeDescOfStat stat)
              , Db.idStatAccess = Just (reason, fileStatDescOfStat stat)
              , Db.idContentAccess = Just (reason, contentDesc)
              }

main :: IO ()
main = do
    let dummyKey = Db.StringKey "dummy"

    testValue <- fmap (const dummyKey) <$> getFileDescInput (Db.BecauseHintFrom []) "/tmp"

    let !encoded = encode testValue
        !executionLog =
            Db.ExecutionLogOf
            { Db.elBuildId = BuildId "dummy"
            , Db.elCommand = dummyKey
            , Db.elInputBranchPath = Db.ELBranchPath . take 1000 $ repeat (dummyKey, testValue)
            , Db.elOutputsDescs = [(dummyKey, FileDescNonExisting ())]
            , Db.elStdoutputs = StdOutputs dummyKey dummyKey
            , Db.elSelfTime = 0
            } :: Db.ExecutionLogOf Db.StringKey
        encodedExecutionLog = encode executionLog

    Db.with "Buildsome.mk.db" $ \db -> do
        let elKey = Db.ExecutionLogNodeKey $ md5 "1234"
            elIRef = Db.executionLogNode elKey db
        defaultMain
            [ bgroup "InputDesc"
              [ bench "encode" $ nf encode testValue
              , bench "decode" $ nf (decode :: ByteString -> Db.InputDescOf Db.StringKey) encoded ]

            , bgroup "ExecutionLogForDb"
              [ bench "encode" $ nf encode executionLog
              , bench "decode" $ nf (decode :: ByteString -> Db.ExecutionLogOf Db.StringKey) encodedExecutionLog ]

            , bgroup "Db"
              [ bench "write" $ nfIO (Db.writeIRef elIRef (Db.ExecutionLogNodeLeaf executionLog))
              , bench "read" $ nfIO (Db.readIRef elIRef)
              ]
            ]

