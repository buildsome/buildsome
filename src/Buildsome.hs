{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Buildsome
  ( Buildsome(bsPhoniesSet), with, withDb
  , clean
  , BuiltTargets(..)
  , PutInputsInStats(..), CollectStats(..), want
  ) where


import qualified Buildsome.BuildId as BuildId
import           Buildsome.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetDesc(..))
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Clean as Clean
import qualified Buildsome.Color as Color
import           Buildsome.Db (Db, IRef(..), Reason)
import qualified Buildsome.Db as Db
import           Buildsome.FileContentDescCache (fileContentDescOfStat)
import qualified Buildsome.MagicFiles as MagicFiles
import qualified Buildsome.Meddling as Meddling
import           Buildsome.Opts (Opt(..))
import qualified Buildsome.Opts as Opts
import qualified Buildsome.Print as Print
import           Buildsome.Slave (Slave)
import qualified Buildsome.Slave as Slave
import           Buildsome.Stats (Stats(Stats))
import qualified Buildsome.Stats as Stats
import           Buildsome.Types (Buildsome(..), Parents, WaitOrCancel(..), BuildTargetEnv(..), BuiltTargets(..), PutInputsInStats(..), CollectStats(..))
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import           Control.Monad (void, unless, when, filterM, forM, forM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, catMaybes, maybeToList)

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Time (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Typeable (Typeable)
import qualified Lib.Cmp as Cmp
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing)
import qualified Lib.Directory as Dir
import           Lib.Exception (finally, logErrors, handle, catch, handleSync, putLn)
import           Lib.FSHook (OutputBehavior(..), OutputEffect(..))
import qualified Lib.FSHook as FSHook
import           Lib.FileDesc (fileModeDescOfStat, fileStatDescOfStat)
import           Lib.FilePath (FilePath, (</>), (<.>))
import qualified Lib.FilePath as FilePath
import qualified Lib.Fresh as Fresh
import           Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import           Lib.Makefile (Makefile(..), TargetType(..), Target, targetAllInputs,
                               targetInterpolatedCmds)
import           Lib.Once (once)
import qualified Lib.Parallelism as Parallelism
import           Lib.Printer (Printer, printStrLn)
import qualified Lib.Printer as Printer
import qualified Lib.Set as LibSet
import           Lib.Show (show)
import           Lib.ShowBytes (showBytes)
import           Lib.Sigint (withInstalledSigintHandler)
import           Lib.StdOutputs (StdOutputs(..))
import qualified Lib.SyncMap as SyncMap
import           Lib.TimeIt (timeIt)
import qualified Lib.Timeout as Timeout
import           System.Exit (ExitCode(..))
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix
import           System.Process (CmdSpec(..))
import           Text.Parsec (SourcePos)

import qualified Prelude.Compat
import           Prelude.Compat hiding (FilePath, show)


onAllSlaves :: WaitOrCancel -> Buildsome -> IO ()
onAllSlaves shouldCancel bs =
  do
    completedRef <- newIORef M.empty
    let
      go alreadyCancelled = do
        curSlaveMap <-
            M.map snd . snd . M.mapEither id <$> SyncMap.toMap (bsSlaveByTargetRep bs)
        curCompleted <- readIORef completedRef
        let liveSlaves = curSlaveMap `M.difference` curCompleted
        unless (M.null liveSlaves) $
          do
            let slavesToCancel = liveSlaves `M.difference` alreadyCancelled
            if M.null slavesToCancel
              then
                -- live slaves but nothing to try and cancel, poll every
                -- 10ms (TODO: Nicer waiting)
                threadDelay 10000
              else
                forM_ (M.toList slavesToCancel) $ \(key, slave) ->
                forkIO $
                do
                  when (shouldCancel == CancelAndWait) $
                    timeoutWarning "cancel" (Timeout.seconds 1) slave $
                    Slave.cancel slave
                  _ <- timeoutWarning "finish" (Timeout.seconds 1) slave $
                    Slave.waitCatch slave
                  return ()
                `finally` atomicModifyIORef'_ completedRef (M.insert key slave)
                `logErrors` BS8.unpack ("cancel of slave " <> bsRender bs (Slave.str slave) <> " failed: ")
            -- Make sure to cancel any potential new slaves that were
            -- created during cancellation
            go curSlaveMap
    go M.empty `logErrors` "BUG: Exception from onAllSlaves: "
  where
    Color.Scheme{..} = Color.scheme
    timeoutWarning str time slave =
      Timeout.warning time $ bsRender bs $
      mconcat ["Slave ", Slave.str slave, " did not ", str, " in ", show time, "!"]

whenVerbose :: Buildsome -> IO () -> IO ()
whenVerbose buildsome = when verbose
  where
    verbose = Opts.verbosityGeneral $ optVerbosity $ bsOpts buildsome

verbosePutLn :: Buildsome -> IO.Handle -> String -> IO ()
verbosePutLn buildsome outHandle str =
  whenVerbose buildsome $ putLn outHandle str

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = FilePath.takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      gitIgnoreBasePath = dir </> gitignoreBaseName
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  verbosePutLn buildsome IO.stdout "Updating .gitignore"
  base <- Dir.catchDoesNotExist
          (fmap BS8.lines $ BS8.readFile $  BS8.unpack gitIgnoreBasePath)
          $ return []
  let
    generatedPaths =
      map (("/" <>) . FilePath.makeRelative dir) $
        extraIgnored ++ S.toList (outputs <> leaked)
  BS8.writeFile (BS8.unpack gitIgnorePath) $ BS8.unlines $
    header ++ generatedPaths ++ base
  where
    gitignoreBaseName = ".gitignore-base"
    header = ["# AUTO GENERATED FILE - DO NOT EDIT",
              BS8.concat
             ["# If you need to ignore files not managed by buildsome, add to ", gitignoreBaseName]]

data ExplicitPathsBuilt = ExplicitPathsBuilt | ExplicitPathsNotBuilt

data ExplicitFileMissing = ExplicitFileMissing (ColorText -> ByteString) FilePath Parents
  deriving (Typeable)
instance E.Exception ExplicitFileMissing
instance Show ExplicitFileMissing where
  show (ExplicitFileMissing render path parents) =
    BS8.unpack $ render $ cError $ mconcat
    [ "ERROR: ", cTarget (show path), " is explicitly demanded but is missing. Rule is either missing or failed to build the output. Dependency chains:\n"
    , showParents parents
    ]
    where
      Color.Scheme{..} = Color.scheme

assertExplicitInputsExist :: BuildTargetEnv -> [FilePath] -> IO ExplicitPathsBuilt
assertExplicitInputsExist BuildTargetEnv{..} paths = do
  res <-
    runExceptT $ forM_ paths $ \path ->
    unless (isPhony bteBuildsome path) $ do
      doesExist <- liftIO $ FilePath.exists path
      unless doesExist $ throwE $
        ExplicitFileMissing (bsRender bteBuildsome) path bteParents
  case res of
    Right () -> return ExplicitPathsBuilt
    Left err | bteExplicitlyDemanded -> E.throwIO err
             | otherwise -> return ExplicitPathsNotBuilt

-- | Top-level root target
want :: Printer -> Buildsome -> CollectStats -> Reason -> [FilePath] -> IO BuiltTargets
want printer buildsome collectStats reason paths = do
  printStrLn printer $
    "Building: " <> ColorText.intercalate ", " (map (cTarget . show) paths)
  let bte =
        BuildTargetEnv
        { bteBuildsome = buildsome
        , btePrinter = printer
        , bteReason = reason
        , bteParents = []
        , bteExplicitlyDemanded = True
        , bteSpeculative = False
        , bteCollectStats = collectStats
        }
  entity <- Parallelism.rootEntity $ bsParPool buildsome
  (buildTime, (ExplicitPathsBuilt, builtTargets)) <-
    timeIt $ buildExplicitWithParReleased bte entity $ map SlaveRequestDirect paths
  let stdErrs = Stats.stdErr $ builtStats builtTargets
      lastLinePrefix
        | not (S.null stdErrs) =
          cWarning $ "Build Successful, but with STDERR from: " <>
          (cTarget . show . map BuildMaps.targetRepPath . S.toList) stdErrs
        | otherwise =
          cSuccess "Build Successful"
  printStrLn printer $ mconcat
    [ lastLinePrefix, ": "
    , cTiming (show buildTime <> " seconds"), " total." ]
  return builtTargets
  where
    Color.Scheme{..} = Color.scheme

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

showParents :: Parents -> ColorText
showParents = mconcat . map showParent
  where
    showParent (targetRep, target, reason) = mconcat
      [ "\n", Print.posText (targetPos target), " "
      , cTarget (show targetRep), " (", show reason, ")"
      ]
    Color.Scheme{..} = Color.scheme

isPhony :: Buildsome -> FilePath -> Bool
isPhony bs path = path `S.member` bsPhoniesSet bs

targetIsPhony :: Buildsome -> Target -> Bool
targetIsPhony bs = all (isPhony bs) . targetOutputs

slaveForDirectPath :: BuildTargetEnv -> FilePath -> IO (Maybe (Parallelism.Entity, Slave Stats))
slaveForDirectPath bte@BuildTargetEnv{..} path
  | FilePath.isAbsolute path =
    -- Only project-relative paths may have output rules:
    return Nothing
  | not bteExplicitlyDemanded && isPhony bteBuildsome path = return Nothing
  | otherwise =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing -> return Nothing
  Just (targetKind, targetDesc) ->
    Just <$>
    getSlaveForTarget
    bte { bteExplicitlyDemanded =
          bteExplicitlyDemanded || targetKind == BuildMaps.TargetSimple }
    targetDesc

slavesForChildrenOf :: BuildTargetEnv -> FilePath -> IO [(Parallelism.Entity, Slave Stats)]
slavesForChildrenOf bte@BuildTargetEnv{..} path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | not (null childPatterns) =
    fail $ "UNSUPPORTED: Read directory on directory with patterns. Path: '" ++ show path ++ "' (" ++ BS8.unpack (bsRender bteBuildsome $ show bteReason) ++ ") Patterns: " ++ show childPatterns
  | otherwise =
    -- Non-pattern targets here, so they're explicitly demanded
    traverse (getSlaveForTarget bte { bteExplicitlyDemanded = True }) $
    -- TODO: targetIsPhony is the wrong question, the right question
    -- is whether the path IN THE DIRECTORY is phony OR we can ban
    -- "partially phony targets"
    filter (not . targetIsPhony bteBuildsome . tdTarget) childTargetDescs
  where
    DirectoryBuildMap childTargetDescs childPatterns =
      BuildMaps.findDirectory (bsBuildMaps bteBuildsome) path

data SlaveRequest
  = SlaveRequestDirect FilePath -- Just the direct path
  | SlaveRequestFull FilePath   -- The path and all underneath it
inputFilePath :: SlaveRequest -> FilePath
inputFilePath (SlaveRequestDirect path) = path
inputFilePath (SlaveRequestFull path) = path

slavesFor :: BuildTargetEnv -> SlaveRequest -> IO [(Parallelism.Entity, Slave Stats)]
slavesFor bte (SlaveRequestDirect path) =
    maybeToList <$> slaveForDirectPath bte path
slavesFor bte@BuildTargetEnv{..} (SlaveRequestFull path) = do
  mSlave <- slaveForDirectPath bte path
  children <-
    slavesForChildrenOf
    bte { bteReason = Db.BecauseChildOfFullyRequestedDirectory bteReason } path
  return $ maybeToList mSlave ++ children

slaveReqForAccessType :: FSHook.AccessType -> FilePath -> SlaveRequest
slaveReqForAccessType FSHook.AccessTypeFull = SlaveRequestFull
slaveReqForAccessType FSHook.AccessTypeModeOnly = SlaveRequestDirect
slaveReqForAccessType FSHook.AccessTypeStat =
  -- This is a (necessary) hack! See KNOWN_ISSUES: "stat of directories"
  SlaveRequestDirect

-- NOTE: To allow for deterministic (and correct-by-priority) command
-- ordering, we must avoid the gap between release of parent slave
-- parallelism and allocation of child slave parallelism.  To do this,
-- we only ever release parallelism locally *after* we've created the
-- slaves, i.e: when waiting for them, in this function.
-- Do NOT release parallelism in any other context!
-- TODO: All 3 applications of this function concat in the same
-- pattern, reduce code duplication
waitForSlavesWithParReleased ::
  BuildTargetEnv -> Parallelism.Entity -> [(Parallelism.Entity, Slave Stats)] -> IO BuiltTargets
waitForSlavesWithParReleased _ _ [] = return mempty
waitForSlavesWithParReleased BuildTargetEnv{..} entity forkedSlaves =
  Parallelism.withReleased btePrinter (bsParPool bteBuildsome) entity forks $
  do
    whenVerbose bteBuildsome $
        Printer.printStrLn btePrinter $
        "Waiting for " <> ColorText.intercalate ", " (map Slave.str slaves)
    stats <- mconcat <$> mapM Slave.wait slaves
    return BuiltTargets { builtTargets = map Slave.target slaves, builtStats = stats }
  where
    forks = map fst forkedSlaves
    slaves = map snd forkedSlaves

buildExplicitWithParReleased ::
  BuildTargetEnv -> Parallelism.Entity -> [SlaveRequest] ->
  IO (ExplicitPathsBuilt, BuiltTargets)
buildExplicitWithParReleased bte@BuildTargetEnv{..} entity inputs = do
  built <-
    waitForSlavesWithParReleased bte entity . concat =<< mapM (slavesFor bte) inputs
  explicitPathsBuilt <- assertExplicitInputsExist bte $ map inputFilePath inputs
  return (explicitPathsBuilt, built)

data IllegalUnspecifiedOutputs = IllegalUnspecifiedOutputs (ColorText -> ByteString) Target [FilePath]
  deriving (Typeable)
instance E.Exception IllegalUnspecifiedOutputs
instance Show IllegalUnspecifiedOutputs where
  show (IllegalUnspecifiedOutputs render target illegalOutputs) =
    BS8.unpack $ render $ cError $ mconcat
    [ "Target: ", cTarget (show (targetOutputs target))
    , " wrote to unspecified output files: ", show illegalOutputs ]
    where
      Color.Scheme{..} = Color.scheme

-- Verify output of whole of slave/execution log
verifyTargetSpec ::
  BuildTargetEnv -> Set FilePath -> Set FilePath -> Target ->
  IO (Set FilePath)
verifyTargetSpec bte inputs outputs target = do
  verifyTargetInputs bte inputs target
  verifyTargetOutputs bte outputs target

verifyTargetInputs :: BuildTargetEnv -> Set FilePath -> Target -> IO ()
verifyTargetInputs bte@BuildTargetEnv{..} inputs target
  | all (`S.member` bsPhoniesSet bteBuildsome)
    (targetOutputs target) = return () -- Phony target doesn't need real inputs
  | otherwise =
    warnOverSpecified bte "inputs" ""
    (S.fromList (targetAllInputs target) `S.difference`
     (inputs `S.union` bsPhoniesSet bteBuildsome))
    (targetPos target)

handleLegalUnspecifiedOutputs :: BuildTargetEnv -> TargetType output input -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs BuildTargetEnv{..} target unspecifiedOutputs = do
  legalUnspecified <- filterM FilePath.exists unspecifiedOutputs
  unless (null legalUnspecified) $ Print.warn btePrinter (targetPos target) $
    mconcat ["Leaked unspecified outputs: ", show legalUnspecified]
  registerLeakedOutputs bteBuildsome $ S.fromList legalUnspecified

-- TODO: Verify phony outputs are not written to
verifyTargetOutputs ::
  BuildTargetEnv -> Set FilePath -> Target -> IO (Set FilePath)
verifyTargetOutputs bte@BuildTargetEnv{..} outputs target = do
  handleLegalUnspecifiedOutputs bte target $ S.toList unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- LibSet.filterA FilePath.exists illegalOutputs
  unless (S.null existingIllegalOutputs) $ do
    Print.posMessage btePrinter (targetPos target) $ cError $
      "Illegal output files: " <> show (S.toList existingIllegalOutputs)

    E.throwIO $
      IllegalUnspecifiedOutputs (bsRender bteBuildsome) target $
      S.toList existingIllegalOutputs
  let unusedOutputs =
        specified `S.difference`
        (outputs `S.union` bsPhoniesSet bteBuildsome)
  warnOverSpecified bte "outputs" " (consider adding a .PHONY declaration)" unusedOutputs (targetPos target)
  mapM_ removeFileOrDirectoryOrNothing $ S.toList unusedOutputs
  return $ S.intersection outputs specified
  where
    Color.Scheme{..} = Color.scheme
    (unspecifiedOutputs, illegalOutputs) =
      S.partition MagicFiles.allowedUnspecifiedOutput
      allUnspecified
    allUnspecified = outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

warnOverSpecified ::
  BuildTargetEnv -> ColorText -> ColorText ->
  Set FilePath -> SourcePos -> IO ()
warnOverSpecified BuildTargetEnv{..} str suffix unused pos =
  unless (S.null unused) $
  Print.warn btePrinter pos $ mconcat
  ["Over-specified ", str, ": ", cTarget (show (S.toList unused)), suffix]
  where
    Color.Scheme{..} = Color.scheme

replayExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  StdOutputs ByteString -> DiffTime -> IO ()
replayExecutionLog bte@BuildTargetEnv{..} target inputs outputs stdOutputs selfTime =
  Print.replay btePrinter target stdOutputs bteReason
  (optVerbosity (bsOpts bteBuildsome)) selfTime $
  -- We only register legal outputs that were CREATED properly in the
  -- execution log, so all outputs keep no old content
  void $ verifyTargetSpec bte inputs outputs target

verifyFileDesc ::
  (IsString str, Monoid str, MonadIO m) =>
  str -> FilePath -> Db.FileDesc ne desc ->
  (Posix.FileStatus -> desc -> ExceptT (str, FilePath) m ()) ->
  ExceptT (str, FilePath) m ()
verifyFileDesc str filePath fileDesc existingVerify = do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case (mStat, fileDesc) of
    (Nothing, Db.FileDescNonExisting _) -> return ()
    (Just stat, Db.FileDescExisting desc) -> existingVerify stat desc
    (Just _, Db.FileDescNonExisting _)  -> throwE (str <> " file did not exist, now exists", filePath)
    (Nothing, Db.FileDescExisting {}) -> throwE (str <> " file was deleted", filePath)

data WrapException = WrapException (ColorText -> ByteString) Parents E.SomeException
  deriving (Typeable)
instance E.Exception WrapException
instance Show WrapException where
  show (WrapException render parents exc) =
    BS8.unpack $ render $ show exc <>
    "\nWhile trying to build:" <> showParents parents
    where
      Color.Scheme{..} = Color.scheme

wrapException :: Buildsome -> Parents -> E.SomeException -> E.SomeException
wrapException buildsome parents exc =
    case E.fromException exc of
    Just WrapException {} -> exc
    Nothing -> E.SomeException (WrapException (bsRender buildsome) parents exc)

unwrapException :: E.SomeException -> E.SomeException
unwrapException exc =
    case E.fromException exc of
    Just (WrapException _ _ inner) -> unwrapException inner
    Nothing -> exc

maybeRedirectExceptions :: BuildTargetEnv -> TargetDesc -> IO a -> IO a
maybeRedirectExceptions BuildTargetEnv{..} TargetDesc{..} =
  handleSync $ \e@E.SomeException {} -> do
    let e' = wrapException bteBuildsome bteParents e
    unless bteSpeculative $
      do
        Printer.printStrLn btePrinter $ cTarget (show tdRep) <> ": " <> cError "Failed"
        bsFastKillBuild bteBuildsome (E.SomeException e')
    E.throwIO e'
  where
    Color.Scheme{..} = Color.scheme

showFirstLine :: (Show a, IsString b) => a -> b
showFirstLine = fromString . concat . take 1 . lines . show

isThreadKilled :: E.SomeException -> Bool
isThreadKilled e =
  case E.fromException (unwrapException e) of
  Just E.ThreadKilled -> True
  _ -> False

syncCatchAndLogSpeculativeErrors :: Printer -> TargetDesc -> (E.SomeException -> a) -> IO a -> IO a
syncCatchAndLogSpeculativeErrors printer TargetDesc{..} errRes =
  handleSync $ \e ->
  do
    unless (isThreadKilled e) $
      Print.posMessage printer (targetPos tdTarget) $ cWarning $
      "Warning: Ignoring failed build of speculative target: " <>
      cTarget (show tdRep) <> " " <> showFirstLine e
    return (errRes e)
  where
    Color.Scheme{..} = Color.scheme

data ExecutionLogFailure
  = MismatchedFiles (ByteString, FilePath)
  | SpeculativeBuildFailure E.SomeException

-- | Map over both failure and success.
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> Db.ExecutionLog ->
  IO (Either ExecutionLogFailure (Db.ExecutionLog, BuiltTargets))
tryApplyExecutionLog bte@BuildTargetEnv{..} entity targetDesc executionLog =
  runExceptT $ do
    builtTargets <-
      ExceptT $
      syncCatchAndLogSpeculativeErrors btePrinter targetDesc
      (Left . SpeculativeBuildFailure)
      (Right <$> executionLogBuildInputs bte entity targetDesc executionLog)
    bimapExceptT MismatchedFiles id $
      executionLogVerifyFilesState bte targetDesc executionLog
    return (executionLog, builtTargets)

executionLogVerifyFilesState ::
  MonadIO m =>
  BuildTargetEnv -> TargetDesc -> Db.ExecutionLog ->
  ExceptT (ByteString, FilePath) m ()
executionLogVerifyFilesState bte@BuildTargetEnv{..} TargetDesc{..} Db.ExecutionLog{..} = do
  forM_ (M.toList elInputsDescs) $ \(filePath, desc) ->
    verifyFileDesc "input" filePath desc $ \stat (mtime, Db.InputDesc mModeAccess mStatAccess mContentAccess) ->
      when (Posix.modificationTimeHiRes stat /= mtime) $ do
        let verify str getDesc mPair =
              verifyMDesc ("input(" <> str <> ")") filePath getDesc $ snd <$> mPair
        verify "mode" (return (fileModeDescOfStat stat)) mModeAccess
        verify "stat" (return (fileStatDescOfStat stat)) mStatAccess
        verify "content"
          (fileContentDescOfStat "When applying execution log (input)"
           db filePath stat) mContentAccess
  -- For now, we don't store the output files' content
  -- anywhere besides the actual output files, so just verify
  -- the output content is still correct
  forM_ (M.toList elOutputsDescs) $ \(filePath, outputDesc) ->
    verifyFileDesc "output" filePath outputDesc $ \stat (Db.OutputDesc oldStatDesc oldMContentDesc) -> do
      verifyDesc  "output(stat)"    filePath (return (fileStatDescOfStat stat)) oldStatDesc
      verifyMDesc "output(content)" filePath
        (fileContentDescOfStat "When applying execution log (output)"
         db filePath stat) oldMContentDesc
  liftIO $
    replayExecutionLog bte tdTarget
    (M.keysSet elInputsDescs) (M.keysSet elOutputsDescs)
    elStdoutputs elSelfTime
  where
    db = bsDb bteBuildsome
    verifyMDesc _   _        _       Nothing        = return ()
    verifyMDesc str filePath getDesc (Just oldDesc) =
      verifyDesc str filePath getDesc oldDesc

    verifyDesc str filePath getDesc oldDesc = do
      newDesc <- liftIO getDesc
      case Cmp.cmp oldDesc newDesc of
        Cmp.Equals -> return ()
        Cmp.NotEquals reasons ->
          -- fail entire computation
          throwE (str <> ": " <> BS8.intercalate ", " reasons, filePath)

executionLogBuildInputs ::
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc ->
  Db.ExecutionLog -> IO BuiltTargets
executionLogBuildInputs bte@BuildTargetEnv{..} entity TargetDesc{..} Db.ExecutionLog {..} = do
  -- TODO: This is good for parallelism, but bad if the set of
  -- inputs changed, as it may build stuff that's no longer
  -- required:
  speculativeSlaves <- concat <$> mapM mkInputSlaves (M.toList elInputsDescs)
  waitForSlavesWithParReleased bte entity speculativeSlaves
  where
    mkInputSlavesFor desc inputPath =
      case fromFileDesc desc of
        Nothing -> return []
        Just (depReason, accessType) ->
          slavesFor bteImplicit
          { bteReason = Db.BecauseSpeculative depReason }
          $ slaveReqForAccessType accessType inputPath
    mkInputSlaves (inputPath, desc)
      | inputPath `S.member` hinted = return []
      | otherwise = mkInputSlavesFor desc inputPath
    Color.Scheme{..} = Color.scheme
    hinted = S.fromList $ targetAllInputs tdTarget
    bteImplicit = bte { bteExplicitlyDemanded = False, bteSpeculative = True }
    fromFileDesc (Db.FileDescNonExisting depReason) =
      -- The access may be larger than mode-only, but we only need to
      -- know if it exists or not because we only need to know whether
      -- the execution log will be re-used or not, not more.
      Just (depReason, FSHook.AccessTypeModeOnly)
    fromFileDesc (Db.FileDescExisting (_mtime, inputDesc)) =
      case inputDesc of
      Db.InputDesc { Db.idContentAccess = Just (depReason, _) } ->
        Just (depReason, FSHook.AccessTypeFull)
      Db.InputDesc { Db.idStatAccess = Just (depReason, _) } ->
        Just (depReason, FSHook.AccessTypeStat)
      Db.InputDesc { Db.idModeAccess = Just (depReason, _) } ->
        Just (depReason, FSHook.AccessTypeModeOnly)
      Db.InputDesc Nothing Nothing Nothing -> Nothing

parentDirs :: [FilePath] -> [FilePath]
parentDirs = map FilePath.takeDirectory . filter (`notElem` ["", "/"])

buildManyWithParReleased ::
  (FilePath -> Reason) -> BuildTargetEnv -> Parallelism.Entity -> [SlaveRequest] -> IO BuiltTargets
buildManyWithParReleased mkReason bte@BuildTargetEnv{..} entity slaveRequests =
  waitForSlavesWithParReleased bte entity =<< fmap concat (mapM mkSlave slaveRequests)
  where
    mkSlave req =
      slavesFor bte { bteReason = mkReason (inputFilePath req) } req
    Color.Scheme{..} = Color.scheme

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> IO (Maybe (Db.ExecutionLog, BuiltTargets))
findApplyExecutionLog bte@BuildTargetEnv{..} entity TargetDesc{..} = do
  mExecutionLog <- readIRef $ Db.executionLog tdTarget $ bsDb bteBuildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return Nothing
    Just executionLog -> do
      eRes <- tryApplyExecutionLog bte entity TargetDesc{..} executionLog
      case eRes of
        Left (SpeculativeBuildFailure exception)
          | isThreadKilled exception -> return Nothing
        Left err -> do
          printStrLn btePrinter $ bsRender bteBuildsome $ mconcat
            [ "Execution log of ", cTarget (show (targetOutputs tdTarget))
            , " did not match because ", describeError err
            ]
          return Nothing
        Right res -> return (Just res)
  where
    Color.Scheme{..} = Color.scheme
    describeError (MismatchedFiles (str, filePath)) =
      fromBytestring8 str <> ": " <> cPath (show filePath)
    describeError (SpeculativeBuildFailure exception) =
      cWarning (showFirstLine exception)

data TargetDependencyLoop = TargetDependencyLoop (ColorText -> ByteString) Parents
  deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop render parents) =
    BS8.unpack $ render $ cError $ fromBytestring8 $
    "Target loop: " <> render (showParents parents)
    where
      Color.Scheme{..} = Color.scheme

data PanicError = PanicError (ColorText -> ByteString) String deriving (Typeable)
instance E.Exception PanicError
instance Show PanicError where
  show (PanicError render msg) =
    BS8.unpack $ render $ cError $ fromString $ "PANIC: " ++ msg
    where
      Color.Scheme{..} = Color.scheme

panic :: (ColorText -> ByteString) -> String -> IO a
panic render msg = do
  putLn IO.stderr $ "PANIC: " ++ msg
  E.throwIO $ PanicError render msg

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: BuildTargetEnv -> TargetDesc -> IO (Parallelism.Entity, Slave Stats)
getSlaveForTarget bte@BuildTargetEnv{..} TargetDesc{..}
  | any ((== tdRep) . fst3) bteParents =
    E.throwIO $ TargetDependencyLoop (bsRender bteBuildsome) newParents
  | otherwise =
    -- SyncMap.insert action should be fully masked (non-blocking or
    -- uninterruptibleMask). The code here is non-blocking, so we
    -- don't need uninterruptibleMask. We cannot use
    -- uninterruptibleMask because the spawned slave's "unmask"
    -- action fully unmasks, whereas we need to become
    -- interruptible-masked (impossible from uninterruptibleMask in
    -- a thread).
    SyncMap.insert (bsSlaveByTargetRep bteBuildsome) tdRep $
    -- We're masked by SyncMap now:
    panicOnError $
    E.uninterruptibleMask $ \restoreUninterruptible ->
    withTimeout $
    do
      -- SyncMap runs this uninterruptible, so should not block
      -- indefinitely.
      depPrinterId <- Fresh.next $ bsFreshPrinterIds bteBuildsome
      depPrinter <- Printer.newFrom btePrinter depPrinterId
      -- NOTE: finishFork MUST be called to avoid leak!
      (fork, wrapChild) <-
          Parallelism.fork (show depPrinterId) btePrinter (bsParPool bteBuildsome)
      slave <-
        restoreUninterruptible $
        Slave.newWithUnmask tdTarget depPrinterId (targetOutputs tdTarget) $
        \unmask ->
            -- Must remain masked through finishFork so it gets a
            -- chance to handle alloc/exception!
            wrapChild depPrinter $ unmask $ do
              let newBte = bte { bteParents = newParents, btePrinter = depPrinter }
              buildTarget newBte fork TargetDesc{..}
      return (fork, slave)
    where
      Color.Scheme{..} = Color.scheme
      newParents = (tdRep, tdTarget, bteReason) : bteParents
      panicHandler e@E.SomeException {} =
        panic (bsRender bteBuildsome) $ "FAILED during making of slave: " ++ show e
      panicOnError = handle panicHandler
      withTimeout =
          Timeout.warning (Timeout.millis 100)
          (Printer.render btePrinter
           ("Slave creation of " <> cTarget (show tdRep) <>
            " took more than 100 millis!"))

data UnregisteredOutputFileExists = UnregisteredOutputFileExists (ColorText -> ByteString) FilePath
  deriving (Typeable)
instance E.Exception UnregisteredOutputFileExists
instance Show UnregisteredOutputFileExists where
  show (UnregisteredOutputFileExists render path) =
    BS8.unpack $ render $ cError $ mconcat
    [ cTarget (show path), " specified as output but exists as a file that "
    , "was not created by buildsome (use --overwrite to go ahead "
    , "anyway)" ]
    where
      Color.Scheme{..} = Color.scheme

removeOldUnregisteredOutput :: Printer -> Buildsome -> FilePath -> IO ()
removeOldUnregisteredOutput printer buildsome path =
  case optOverwriteUnregisteredOutputs (bsOpts buildsome) of
  Opts.DontOverwriteUnregisteredOutputs ->
    E.throwIO $ UnregisteredOutputFileExists (bsRender buildsome) path
  Opts.OverwriteUnregisteredOutputs -> do
    Printer.printStrLn printer $
      "Overwriting " <> cTarget (show path) <> " (due to --overwrite)"
    removeFileOrDirectory path
  where
    Color.Scheme{..} = Color.scheme

removeOldOutput :: Printer -> Buildsome -> Set FilePath -> FilePath -> IO ()
removeOldOutput printer buildsome registeredOutputs path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> return () -- Nothing to do
    Just _
      | path `S.member` registeredOutputs -> removeFileOrDirectory path
      | otherwise -> removeOldUnregisteredOutput printer buildsome path

data RunCmdResults = RunCmdResults
  { rcrSelfTime :: DiffTime -- excluding pause times
  , rcrStdOutputs :: StdOutputs ByteString
  , rcrInputs :: Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)
  , rcrOutputs :: Map FilePath Reason
  , rcrBuiltTargets :: BuiltTargets
  }

outEffectToMaybe :: OutputEffect -> Bool
outEffectToMaybe OutputEffectNone = False
outEffectToMaybe OutputEffectChanged = True

delayedOutputEffect :: FSHook.DelayedOutput -> IO Bool
delayedOutputEffect (FSHook.DelayedOutput behavior path) = do
  outputExists <- FilePath.exists path
  return $ outEffectToMaybe $
    if outputExists
    then whenFileExists behavior
    else whenFileMissing behavior

undelayedOutputEffect :: FSHook.OutFilePath -> Bool
undelayedOutputEffect (FSHook.OutFilePath _ effect) =
  case effect of
  FSHook.OutEffectNothing -> False
  _ -> True

recordInput ::
  IORef (Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)) ->
  Reason -> FSHook.Input -> IO ()
recordInput inputsRef reason (FSHook.Input accessType path) = do
  mStat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $ M.insertWith merge path
    (newAccessTypes, mStat)
  where
    newAccessTypes = M.singleton accessType reason
    -- TODO: We'd like to verify no meddling of the create-delete,
    -- which would go undetected, lacking mtime via:

    -- merge (_, Just _) (_, Nothing) = ...

    -- However, we must then verify that it wasn't a legal output file
    -- of the same job
    merge _ (oldAccessTypes, oldMStat) = (oldAccessTypes `mappend` newAccessTypes, oldMStat)

recordOutputs ::
  Buildsome ->
  IORef (Map FilePath Reason) -> Reason ->
  Set FilePath -> Set FilePath -> IO ()
recordOutputs buildsome outputsRef accessDoc targetOutputsSet paths = do
  atomicModifyIORef'_ outputsRef $ mappend $ M.fromSet (const accessDoc) paths
  registerOutputs buildsome $ paths `S.intersection` targetOutputsSet

makeImplicitInputs ::
  IORef BuiltTargets -> BuildTargetEnv -> Parallelism.Entity -> Reason ->
  [FSHook.Input] -> [FSHook.DelayedOutput] -> IO ()
makeImplicitInputs builtTargetsRef bte@BuildTargetEnv{..} entity accessDoc inputs outputs =
  do
    targetParentsBuilt <-
      buildManyWithParReleased (Db.BecauseContainerDirectoryOfInput accessDoc)
      bteImplicit entity $ map SlaveRequestDirect $ parentDirs allPaths

    inputsBuilt <-
      buildManyWithParReleased (Db.BecauseInput accessDoc)
      bteImplicit entity $ map slaveReqForHookInput inputs

    atomicModifyIORef_ builtTargetsRef (<> (targetParentsBuilt <> inputsBuilt))
  where
    slaveReqForHookInput (FSHook.Input accessType path) =
      slaveReqForAccessType accessType path
    allPaths = map FSHook.inputPath inputs ++ map FSHook.outputPath outputs
    bteImplicit = bte { bteExplicitlyDemanded = False }

fsAccessHandlers ::
  IORef (Map FilePath Reason) ->
  IORef (Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus)) ->
  IORef BuiltTargets ->
  BuildTargetEnv ->
  Parallelism.Entity ->
  TargetType FilePath input ->
  FSHook.FSAccessHandlers
fsAccessHandlers outputsRef inputsRef builtTargetsRef bte@BuildTargetEnv{..} entity target =
    FSHook.FSAccessHandlers
    { delayedFSAccessHandler = fsDelayedAccessHandler
    , undelayedFSAccessHandler = fsUndelayedAccessHandler
    , traceHandler = Print.hookTrace btePrinter
    }
  where
    fsUndelayedAccessHandler accessDoc rawInputs rawOutputs =
      commonAccessHandler accessDoc rawInputs rawOutputs
      FSHook.outPath (return . undelayedOutputEffect) $ \_ _ -> return ()

    fsDelayedAccessHandler accessDoc rawInputs rawOutputs =
      commonAccessHandler accessDoc rawInputs rawOutputs
      FSHook.outputPath delayedOutputEffect $
      makeImplicitInputs builtTargetsRef bte entity $ Db.BecauseHooked accessDoc

    targetOutputsSet = S.fromList $ targetOutputs target

    inputIgnored recordedOutputs (FSHook.Input _ path) =
      MagicFiles.inputIgnored path ||
      path `M.member` recordedOutputs ||
      path `S.member` targetOutputsSet

    commonAccessHandler accessDoc rawInputs rawOutputs
      getOutPath getOutEffect handler
      = do
        recordedOutputs <- readIORef outputsRef
        let ignoredOutput = MagicFiles.outputIgnored . getOutPath
            ignoredInput = inputIgnored recordedOutputs
            inputs = filter (not . ignoredInput) rawInputs
            outputs = filter (not . ignoredOutput) rawOutputs
            reason = Db.BecauseHooked accessDoc
        () <- handler inputs outputs
        let addMEffect output = do
              hasEffect <- getOutEffect output
              return $ if hasEffect
                then Just $ getOutPath output
                else Nothing
        filteredOutputs <- S.fromList . catMaybes <$> mapM addMEffect outputs
        recordOutputs bteBuildsome outputsRef reason
          targetOutputsSet filteredOutputs
        mapM_ (recordInput inputsRef reason) $
          filter ((`M.notMember` recordedOutputs) . FSHook.inputPath) inputs

runCmd :: BuildTargetEnv -> Parallelism.Entity -> Target -> IO RunCmdResults
runCmd bte@BuildTargetEnv{..} entity target = do
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  builtTargetsRef <- newIORef mempty
  let accessHandlers =
          fsAccessHandlers outputsRef inputsRef builtTargetsRef bte entity target
  Parallelism.upgradePriority btePrinter entity
  (time, (exitCode, stdOutputs)) <-
    -- TODO we should record what the values of these env vars were,
    -- and if they change - rebuild
    FSHook.timedRunCommand hook rootPath ["HOME", "PATH"] shellCmd
    (targetOutputs target)
    (cTarget (show (targetOutputs target))) accessHandlers
  case exitCode of
    ExitFailure {} ->
      E.throwIO $ TargetCommandFailed (bsRender bteBuildsome) target exitCode stdOutputs
    _ -> return ()
  Print.targetStdOutputs btePrinter target stdOutputs
  inputs <- readIORef inputsRef
  outputs <- readIORef outputsRef
  builtTargets <- readIORef builtTargetsRef
  return RunCmdResults
    { rcrStdOutputs = stdOutputs
    , rcrSelfTime = realToFrac time
    , rcrInputs =
      -- This is because we don't serialize input/output access
      -- (especially when undelayed!) to the same file. So we don't
      -- really have a correct input stat/desc here for such inputs. However, outputs should always considered as mode-only inputs.
      inputs `M.difference` outputs
    , rcrOutputs = outputs
    , rcrBuiltTargets = builtTargets
    }
  where
    rootPath = bsRootPath bteBuildsome
    hook = bsFsHook bteBuildsome
    shellCmd = ShellCommand (BS8.unpack (targetInterpolatedCmds target))
    Color.Scheme{..} = Color.scheme

makeExecutionLog ::
  Buildsome -> Target ->
  Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
  [FilePath] -> StdOutputs ByteString -> DiffTime -> IO Db.ExecutionLog
makeExecutionLog buildsome target inputs outputs stdOutputs selfTime = do
  inputsDescs <- M.traverseWithKey inputAccess inputs
  outputDescPairs <-
    forM outputs $ \outPath -> do
      mStat <- Dir.getMFileStatus outPath
      fileDesc <-
        case mStat of
        Nothing -> return $ Db.FileDescNonExisting ()
        Just stat -> do
          mContentDesc <-
            if Posix.isDirectory stat
            then return Nothing
            else Just <$>
                 fileContentDescOfStat "When making execution log (output)"
                 db outPath stat
          return $ Db.FileDescExisting $ Db.OutputDesc (fileStatDescOfStat stat) mContentDesc
      return (outPath, fileDesc)
  return Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elCommand = targetInterpolatedCmds target
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = stdOutputs
    , elSelfTime = selfTime
    }
  where
    db = bsDb buildsome
    assertFileMTime path oldMStat =
      unless (MagicFiles.allowedUnspecifiedOutput path) $
      Meddling.assertFileMTime "When making execution log (input)" path oldMStat
    inputAccess ::
      FilePath ->
      (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
      IO (Db.FileDesc Reason (POSIXTime, Db.InputDesc))
    inputAccess path (accessTypes, Nothing) = do
      let reason =
            case M.elems accessTypes of
            [] -> error $ "AccessTypes empty in rcrInputs:" ++ show path
            x:_ -> x
      assertFileMTime path Nothing
      return $ Db.FileDescNonExisting reason
    inputAccess path (accessTypes, Just stat) = do
      assertFileMTime path $ Just stat
      let
        addDesc accessType getDesc = do
          desc <- getDesc
          return $ flip (,) desc <$> M.lookup accessType accessTypes
      -- TODO: Missing meddling check for non-contents below!
      modeAccess <-
        addDesc FSHook.AccessTypeModeOnly $ return $ fileModeDescOfStat stat
      statAccess <-
        addDesc FSHook.AccessTypeStat $ return $ fileStatDescOfStat stat
      contentAccess <-
        addDesc FSHook.AccessTypeFull $
        fileContentDescOfStat "When making execution log (input)" db path stat
      return $ Db.FileDescExisting
        ( Posix.modificationTimeHiRes stat
        , Db.InputDesc
          { Db.idModeAccess = modeAccess
          , Db.idStatAccess = statAccess
          , Db.idContentAccess = contentAccess
          }
        )

deleteOldTargetOutputs :: BuildTargetEnv -> TargetType FilePath input -> IO ()
deleteOldTargetOutputs BuildTargetEnv{..} target = do
  registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb bteBuildsome
  mapM_ (removeOldOutput btePrinter bteBuildsome registeredOutputs) $
    targetOutputs target

buildTargetHints ::
  BuildTargetEnv -> Parallelism.Entity -> TargetType FilePath FilePath -> IO (ExplicitPathsBuilt, BuiltTargets)
buildTargetHints bte@BuildTargetEnv{..} entity target =
  do
    let parentPaths = parentDirs $ targetOutputs target
    targetParentsBuilt <-
      buildManyWithParReleased Db.BecauseContainerDirectoryOfOutput bte
      entity $ map SlaveRequestDirect parentPaths
    explicitParentsBuilt <- assertExplicitInputsExist bte parentPaths
    case explicitParentsBuilt of
      ExplicitPathsNotBuilt -> return (ExplicitPathsNotBuilt, targetParentsBuilt)
      ExplicitPathsBuilt -> do
        (explicitPathsBuilt, inputsBuilt) <-
          buildExplicitWithParReleased
            bte { bteReason = Db.BecauseHintFrom $ targetOutputs target }
            entity $ map SlaveRequestDirect $ targetAllInputs target
        return (explicitPathsBuilt, targetParentsBuilt <> inputsBuilt)
  where
    Color.Scheme{..} = Color.scheme

buildTargetReal ::
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> IO (Db.ExecutionLog, BuiltTargets)
buildTargetReal bte@BuildTargetEnv{..} entity TargetDesc{..} =
  Print.targetWrap btePrinter bteReason tdTarget "BUILDING" $ do
    deleteOldTargetOutputs bte tdTarget

    Print.executionCmd verbosityCommands btePrinter tdTarget

    RunCmdResults{..} <- runCmd bte entity tdTarget

    outputs <- verifyTargetSpec bte (M.keysSet rcrInputs) (M.keysSet rcrOutputs) tdTarget
    executionLog <-
      makeExecutionLog bteBuildsome tdTarget rcrInputs (S.toList outputs)
      rcrStdOutputs rcrSelfTime
    writeIRef (Db.executionLog tdTarget (bsDb bteBuildsome)) executionLog

    Print.targetTiming btePrinter "now" rcrSelfTime
    return (executionLog, rcrBuiltTargets)
  where
    verbosityCommands = Opts.verbosityCommands verbosity
    verbosity = optVerbosity (bsOpts bteBuildsome)

statsOfNullCmd :: BuildTargetEnv -> TargetDesc -> BuiltTargets -> Stats
statsOfNullCmd BuildTargetEnv{..} TargetDesc{..} hintedBuiltTargets =
  stats <> Stats
  { Stats.ofTarget =
    M.singleton tdRep Stats.TargetStats
    { tsWhen = Stats.BuiltNow
    , tsTime = 0
    , tsDirectDeps = deps
    , tsExistingInputs =
      case bteCollectStats of
      CollectStats PutInputsInStats -> Just $ targetAllInputs tdTarget
      _ -> Nothing
    }
  , Stats.stdErr = S.empty
  }
  where
    BuiltTargets deps stats = hintedBuiltTargets

buildTarget :: BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> IO Stats
buildTarget bte@BuildTargetEnv{..} entity TargetDesc{..} =
  maybeRedirectExceptions bte TargetDesc{..} $ do
    (explicitPathsBuilt, hintedBuiltTargets) <- buildTargetHints bte entity tdTarget
    case explicitPathsBuilt of
      ExplicitPathsNotBuilt ->
        -- Failed to build our hints when allowed, just leave with collected stats
        return $ builtStats hintedBuiltTargets
      ExplicitPathsBuilt | BS8.null $ targetInterpolatedCmds tdTarget ->
        return $ statsOfNullCmd bte TargetDesc{..} hintedBuiltTargets
      ExplicitPathsBuilt | otherwise ->  do
        mSlaveStats <- findApplyExecutionLog bte entity TargetDesc{..}
        (whenBuilt, (Db.ExecutionLog{..}, builtTargets)) <-
          case mSlaveStats of
          Just res -> return (Stats.FromCache, res)
          Nothing -> (,) Stats.BuiltNow <$> buildTargetReal bte entity TargetDesc{..}
        return $! -- strict application, otherwise stuff below isn't
                  -- gc'd apparently.
          case bteCollectStats of
            Don'tCollectStats -> mempty
            CollectStats putInputsInStats ->
              let BuiltTargets deps stats = hintedBuiltTargets <> builtTargets
              in  stats <>
                  Stats
                  { Stats.ofTarget =
                    M.singleton tdRep Stats.TargetStats
                    { tsWhen = whenBuilt
                    , tsTime = elSelfTime
                    , tsDirectDeps = deps
                    , tsExistingInputs =
                      case putInputsInStats of
                      PutInputsInStats ->
                          Just $ targetAllInputs tdTarget ++ [ path | (path, Db.FileDescExisting _) <- M.toList elInputsDescs ]
                      Don'tPutInputsInStats -> Nothing
                    }
                  , Stats.stdErr =
                    if mempty /= stdErr elStdoutputs
                    then S.singleton tdRep
                    else S.empty
                  }
  where
    Color.Scheme{..} = Color.scheme

registerDbList :: Ord a => (Db -> IORef (Set a)) -> Buildsome -> Set a -> IO ()
registerDbList mkIORef buildsome newItems =
  atomicModifyIORef'_ (mkIORef (bsDb buildsome)) (newItems <>)

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs = registerDbList Db.registeredOutputsRef

registerLeakedOutputs :: Buildsome -> Set FilePath -> IO ()
registerLeakedOutputs = registerDbList Db.leakedOutputsRef

{-# INLINE andM #-}
andM :: Monad m => (a -> m Bool) -> [a] -> m Bool
andM check = go
    where
        go [] = return True
        go (x:xs) =
            do
                res <- check x
                if res
                    then go xs
                    else return False

data FileBuildRule
  = NoBuildRule
  | PhonyBuildRule
  | ValidBuildRule
  | InvalidPatternBuildRule {- transitively missing inputs -}
  deriving (Eq)
getFileBuildRule :: Set FilePath -> Set FilePath -> BuildMaps -> FilePath -> IO FileBuildRule
getFileBuildRule registeredOutputs phonies buildMaps = go
  where
    go path
      | path `S.member` phonies = return PhonyBuildRule
      | otherwise =
        case BuildMaps.find buildMaps path of
        Nothing -> return NoBuildRule
        Just (BuildMaps.TargetSimple, _) -> return ValidBuildRule
        Just (BuildMaps.TargetPattern, targetDesc) ->
          do
            inputsCanExist <- andM fileCanExist (targetAllInputs (tdTarget targetDesc))
            return $
              if inputsCanExist
              then ValidBuildRule
              else InvalidPatternBuildRule
    fileCanExist path = do
      fileBuildRule <- go path
      case fileBuildRule of
        InvalidPatternBuildRule -> return False
        PhonyBuildRule -> return False
        ValidBuildRule -> return True
        NoBuildRule
          | path `S.member` registeredOutputs -> return False -- a has-been
          | otherwise -> FilePath.exists path

deleteRemovedOutputs :: Printer -> Db -> Set FilePath -> BuildMaps -> IO ()
deleteRemovedOutputs printer db phonies buildMaps = do
  -- No need for IORef atomicity here, this happens strictly before
  -- anything else, without parallelism
  oldRegisteredOutputs <- readIORef (Db.registeredOutputsRef db)
  (newRegisteredOutputs, toDelete) <-
    LibSet.partitionA
    (fmap (== ValidBuildRule) .
     getFileBuildRule oldRegisteredOutputs phonies buildMaps)
    oldRegisteredOutputs
  writeIORef (Db.registeredOutputsRef db) newRegisteredOutputs
  forM_ (S.toList toDelete) $ \path -> do
    Printer.rawPrintStrLn printer $ "Removing old output: " <> cPath (show path)
    removeFileOrDirectoryOrNothing path
  where
    Color.Scheme{..} = Color.scheme

data TargetCommandFailed = TargetCommandFailed (ColorText -> ByteString) Target ExitCode (StdOutputs ByteString) deriving (Typeable)
instance E.Exception TargetCommandFailed
instance Show TargetCommandFailed where
  show (TargetCommandFailed render target exitCode stdOutputs) =
    BS8.unpack $ render $ cError $ mconcat
    [ fromBytestring8 (Print.delimitMultiline cmd), " failed: ", show exitCode
    , maybe "" ("\n" <>) $ Print.outputsStr stdOutputs ]
    where
      Color.Scheme{..} = Color.scheme
      cmd = targetInterpolatedCmds target

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

withDb :: FilePath -> (Db -> IO a) -> IO a
withDb makefilePath = Db.with $ buildDbFilename makefilePath


with ::
  Printer -> Db -> FilePath -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
with printer db makefilePath makefile opt@Opt{..} body = do
  ldPreloadPath <- FSHook.getLdPreloadPath optFsOverrideLdPreloadPath
  FSHook.with printer ldPreloadPath $ \fsHook -> do
    slaveMapByTargetRep <- SyncMap.new
    -- Many, many slaves are invoked, but only up to optParallelism
    -- external processes are invoked in parallel. The Parallelism lib
    -- (in Lib/Parallelism) is used by slaves to allocate parallelism
    -- tokens, up to optParallelism tokens at once.
    pool <- Parallelism.newPool $ fromMaybe 1 optParallelism
    freshPrinterIds <- Fresh.new $ Printer.Id 1
    buildId <- BuildId.new
    rootPath <- FilePath.canonicalizePath $ FilePath.takeDirectory makefilePath
    let buildMaps = BuildMaps.make makefile
    let phoniesSet = S.fromList . map snd $ makefilePhonies makefile
    deleteRemovedOutputs printer db phoniesSet buildMaps

    runOnce <- once
    errorRef <- newIORef Nothing
    let
      killOnce msg exception =
        void $ E.uninterruptibleMask_ $ runOnce $ do
          putLn IO.stderr msg
          atomicModifyIORef_ errorRef $ maybe (Just exception) Just
          forkIO $ onAllSlaves CancelAndWait buildsome
      buildsome =
        Buildsome
        { bsOpts = opt
        , bsMakefile = makefile
        , bsPhoniesSet = phoniesSet
        , bsBuildId = buildId
        , bsRootPath = rootPath
        , bsBuildMaps = buildMaps
        , bsDb = db
        , bsFsHook = fsHook
        , bsSlaveByTargetRep = slaveMapByTargetRep
        , bsFreshPrinterIds = freshPrinterIds
        , bsFastKillBuild = case optKeepGoing of
            Opts.KeepGoing -> const (return ())
            Opts.DieQuickly -> killOnce "Build step failed, no -k specified"
        , bsRender = Printer.render printer
        , bsParPool = pool
        }
    withInstalledSigintHandler
      (killOnce "\nBuild interrupted by Ctrl-C, shutting down."
       (E.SomeException E.UserInterrupt)) $
      body buildsome
        -- We must not leak running slaves as we're not allowed to
        -- access fsHook, db, etc after leaving here:
        `finally` onAllSlaves Wait buildsome
        -- Must update gitIgnore after all the slaves finished updating
        -- the registered output lists:
        `finally` maybeUpdateGitIgnore buildsome
        `catch` \e -> E.throwIO . fromMaybe e =<< readIORef errorRef
  where
    maybeUpdateGitIgnore buildsome =
      case optUpdateGitIgnore of
      Opts.UpdateGitIgnore -> updateGitIgnore buildsome makefilePath
      Opts.DontUpdateGitIgnore -> return ()

clean :: Printer -> Buildsome -> IO ()
clean printer buildsome = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  Clean.Result _totalSize totalSpace count <-
    mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
  writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
  writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
  Printer.rawPrintStrLn printer $ mconcat
    [ cSuccess "Clean Successful", ": Cleaned "
    , show count, " files freeing an estimated "
    , showBytes (fromIntegral totalSpace)
    ]
  where
    Color.Scheme{..} = Color.scheme
