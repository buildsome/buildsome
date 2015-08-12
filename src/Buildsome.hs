{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards, OverloadedStrings, TupleSections, FlexibleInstances, CPP #-}
module Buildsome
  ( Buildsome(bsPhoniesSet), with, withDb
  , clean
  , BuiltTargets(..)
  , want
  ) where

import qualified Prelude.Compat
import           Prelude.Compat hiding (FilePath, show)

import           Buildsome.BuildId (BuildId)
import qualified Buildsome.BuildId as BuildId
import           Buildsome.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
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
import Control.Applicative ((<|>))
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import           Control.Monad (void, unless, when, filterM, forM, forM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Either (EitherT(..), left, bimapEitherT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, catMaybes, maybeToList)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Time (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixDayLength)
import           Data.Typeable (Typeable)
import           Lib.Cmp (Cmp)
import qualified Lib.Cmp as Cmp
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing)
import qualified Lib.Directory as Dir
import           Lib.Exception (finally, logErrors, handle, catch, handleSync, putLn)
import           Lib.FSHook (FSHook, OutputBehavior(..), OutputEffect(..))
import qualified Lib.FSHook as FSHook
import           Lib.FileDesc (fileModeDescOfStat, fileStatDescOfStat, FileContentDesc(..), FileStatDesc(..), BasicStatEssence(..), FullStatEssence(..))
import           Lib.FilePath (FilePath, (</>), (<.>))
import qualified Lib.FilePath as FilePath
import           Lib.Fresh (Fresh)
import qualified Lib.Fresh as Fresh
import           Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import           Lib.Makefile (Makefile(..), TargetType(..), Target, targetAllInputs)
import qualified Lib.NonEmptyList as NonEmptyList
import           Lib.Once (once)
import qualified Lib.Parallelism as Parallelism
import           Lib.Printer (Printer, printStrLn)
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Set as LibSet
import           Lib.Show (show)
import           Lib.ShowBytes (showBytes)
import           Lib.Sigint (withInstalledSigintHandler)
import           Lib.StdOutputs (StdOutputs(..))
import           Lib.SyncMap (SyncMap)
import qualified Lib.SyncMap as SyncMap
import           Lib.TimeIt (timeIt)
import qualified Lib.Timeout as Timeout
import           System.Exit (ExitCode(..))
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix
import           System.Process (CmdSpec(..))
import qualified Data.ByteString.Base16 as Base16
import           Text.Parsec (SourcePos)

const_CACHED_OUTPUTS_DIR :: FilePath
const_CACHED_OUTPUTS_DIR = "cached_outputs"
const_MAX_CACHE_FILE_AGE :: POSIXTime
const_MAX_CACHE_FILE_AGE = posixDayLength * 5

type Parents = [(TargetRep, Target, Reason)]

data Buildsome = Buildsome
  { -- static:
    bsOpts :: Opt
  , bsMakefile :: Makefile
  , bsBuildsomePath :: FilePath
  , bsPhoniesSet :: Set FilePath
  , bsBuildId :: BuildId
  , bsRootPath :: FilePath
  , bsBuildMaps :: BuildMaps
    -- dynamic:
  , bsDb :: Db
  , bsFsHook :: FSHook
  , bsSlaveByTargetRep :: SyncMap TargetRep (Parallelism.Entity, Slave Stats)
  , bsFreshPrinterIds :: Fresh Printer.Id
  , bsFastKillBuild :: E.SomeException -> IO ()
  , bsRender :: ColorText -> ByteString
  , bsParPool :: Parallelism.Pool
  }

data WaitOrCancel = Wait | CancelAndWait
  deriving Eq

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

data BuildTargetEnv = BuildTargetEnv
  { bteBuildsome :: Buildsome
  , btePrinter :: Printer
  , bteReason :: Reason
  , bteParents :: Parents
  , bteExplicitlyDemanded :: Bool
  , bteSpeculative :: Bool
  }

data BuiltTargets = BuiltTargets
  { builtTargets :: [Target]
  , builtStats :: Stats
  }
instance Monoid BuiltTargets where
  mempty = BuiltTargets mempty mempty
  mappend (BuiltTargets a1 b1) (BuiltTargets a2 b2) =
    BuiltTargets (mappend a1 a2) (mappend b1 b2)

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
    runEitherT $ forM_ paths $ \path ->
    unless (isPhony bteBuildsome path) $ do
      doesExist <- liftIO $ FilePath.exists path
      unless doesExist $ left $
        ExplicitFileMissing (bsRender bteBuildsome) path bteParents
  case res of
    Right () -> return ExplicitPathsBuilt
    Left err | bteExplicitlyDemanded -> E.throwIO err
             | otherwise -> return ExplicitPathsNotBuilt

-- | Top-level root target
want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO BuiltTargets
want printer buildsome reason paths = do
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
  | otherwise =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing -> return Nothing
  Just (targetRep, targetKind, target) ->
    Just <$>
    getSlaveForTarget
    bte { bteExplicitlyDemanded =
          bteExplicitlyDemanded || targetKind == BuildMaps.TargetSimple }
    (TargetDesc targetRep target)

slavesForChildrenOf :: BuildTargetEnv -> FilePath -> IO [(Parallelism.Entity, Slave Stats)]
slavesForChildrenOf bte@BuildTargetEnv{..} path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | not (null childPatterns) =
    fail $ "UNSUPPORTED: Read directory on directory with patterns: " ++ show path ++ " (" ++ BS8.unpack (bsRender bteBuildsome $ show bteReason) ++ ")"
  | otherwise =
    -- Non-pattern targets here, so they're explicitly demanded
    traverse (getSlaveForTarget bte { bteExplicitlyDemanded = True }) $
    filter (not . targetIsPhony bteBuildsome . tdTarget) childTargetDescs
  where
    childTargetDescs = map (uncurry TargetDesc) childTargets
    DirectoryBuildMap childTargets childPatterns =
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
  children <- slavesForChildrenOf bte path
              -- TODO: { bteReason = bteReason <> " (Container directory)" }
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

-- TODO: rename to replay?
replayExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  ByteString -> DiffTime -> IO ()
replayExecutionLog bte@BuildTargetEnv{..} target inputs outputs stdErr selfTime =
  Print.replay btePrinter target stdErr bteReason
  (optVerbosity (bsOpts bteBuildsome)) selfTime $
  -- We only register legal outputs that were CREATED properly in the
  -- execution log, so all outputs keep no old content
  void $ verifyTargetSpec bte inputs outputs target

verifyFileDesc :: (IsString str, MonadIO m)
  => BuildTargetEnv
  -> FilePath
  -> Db.FileDesc ne (POSIXTime, Db.OutputDesc)
  -> (Posix.FileStatus -> Db.OutputDesc -> (EitherT ByteString IO ()))
  -> EitherT str m ()
verifyFileDesc bte@BuildTargetEnv{..} filePath fileDesc existingVerify = do
  let restore (Db.OutputDesc oldStatDesc oldMContentDesc) =
        refreshFromContentCache bte filePath oldMContentDesc (Just oldStatDesc)
      remove = liftIO . removeFileOrDirectoryOrNothing
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case (mStat, fileDesc) of
    (Nothing, Db.FileDescNonExisting _) -> return ()
    (Just stat, Db.FileDescExisting (mtime, desc))
      | Posix.modificationTimeHiRes stat /= mtime -> do
          res <- liftIO $ runEitherT $ existingVerify stat desc
          case res of
            Left reason -> do
              restore desc
              explain $ mconcat
                [ "File exists but is wrong ("
                , ColorText.simple reason
                , "): restored from cache "
                ]
            Right x -> return x
      | otherwise -> return ()
    (Just _, Db.FileDescNonExisting _)  -> do
      remove filePath
      explain "File exists, but shouldn't: removed "
    (Nothing, Db.FileDescExisting (_, desc)) -> do
      restore desc
      explain "File doesn't exist, but should: restored from cache "
  where
    explain msg = liftIO $ printStrLn btePrinter
      $ bsRender bteBuildsome $ mconcat
      $ [ msg, cPath (show filePath) ]
    Color.Scheme{..} = Color.scheme

data TargetDesc = TargetDesc
  { tdRep :: TargetRep
  , tdTarget :: Target
  }

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

tryApplyExecutionLogSingle ::
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> Db.ExecutionFilesSummary
  -> EitherT [ExecutionLogFailure] IO (Db.ExecutionFilesSummary, BuiltTargets)
tryApplyExecutionLogSingle bte@BuildTargetEnv{..} entity targetDesc efs@Db.ExecutionFilesSummary{..} = do
  builtTargets <-
    EitherT $
    syncCatchAndLogSpeculativeErrors btePrinter targetDesc
    (\x -> Left [SpeculativeBuildFailure x])
    (Right <$>
     executionLogBuildInputs bte entity targetDesc efsInputsDescs)
  bimapEitherT (\x -> [MismatchedFiles x]) id $
    executionSummaryVerifyFilesState bte targetDesc efs
  return (efs, builtTargets)

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> Db.ExecutionSummary
  -> IO (Either [ExecutionLogFailure] (Db.ExecutionFilesSummary, BuiltTargets))
tryApplyExecutionLog bte entity targetDesc Db.ExecutionSummary{..} = runEitherT $
  foldr (<|>) (applyLog $ NonEmptyList.neHead esFilesSummary) $ map applyLog $ NonEmptyList.neTail esFilesSummary
  where applyLog = tryApplyExecutionLogSingle bte entity targetDesc . snd

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
      Just fileStatus -> do
        if (Posix.modificationTimeHiRes fileStatus - currentTime > const_MAX_CACHE_FILE_AGE)
        then do
          removeFileOrDirectoryOrNothing fileName
          return $ Just fileName
        else
          return Nothing
  let numRemoved = length $ catMaybes removed
  putStrLn $ concat
    [ "Cache contains ", show (length removed)
    , ", removed ", show numRemoved, " old cache files" ]


mkTargetWithHashPath :: Buildsome -> ByteString -> FilePath
mkTargetWithHashPath buildsome contentHash = contentCacheDir buildsome </> Base16.encode contentHash-- (outPath <> "." <> Base16.encode contentHash)

refreshFromContentCache :: (IsString e, MonadIO m) =>
  BuildTargetEnv -> FilePath -> Maybe FileContentDesc -> Maybe FileStatDesc -> EitherT e m ()
refreshFromContentCache
  BuildTargetEnv{..} filePath (Just (FileContentDescRegular contentHash)) (Just (FileStatOther fullStat)) = do
  liftIO (FilePath.exists cachedPath) >>= \oldExists ->
    unless oldExists $ left "No cached copy"
  liftIO $ do
    printStrLn btePrinter $ bsRender bteBuildsome
      $ mconcat [ "Copying: " <> cPath (show cachedPath) <> " -> " <> cPath (show filePath) ]
    FilePath.exists filePath >>= \newExists ->
       when newExists $ removeFileOrDirectoryOrNothing filePath
    Dir.createDirectories $ FilePath.takeDirectory filePath
    -- Set the stat attributes before creating the file, so the target file is created with correct
    -- attrs from the start
    -- TODO use proper tempfile naming
    let tempFile = filePath <> "._buildsome_temp"
    Dir.copyFile cachedPath  tempFile
    -- TODO set other stat fields?
    -- TODO these may cause failure later (over permissions) if the user running now is not the one recorded in the log!
    Posix.setFileMode tempFile (fileMode $ basicStatEssence fullStat)
    Posix.setOwnerAndGroup tempFile (fileOwner $ basicStatEssence fullStat) (fileGroup $ basicStatEssence fullStat)
    Dir.renameFile tempFile filePath
    -- Setting file times must be the last thing we do
    Posix.setFileTimesHiRes tempFile (statusChangeTimeHiRes fullStat) (modificationTimeHiRes fullStat)

  where Color.Scheme{..} = Color.scheme
        cachedPath = mkTargetWithHashPath bteBuildsome contentHash
refreshFromContentCache _ _ _ _ = left "No cached info"

verifyMDesc :: (MonadIO m, Cmp a) => ByteString -> IO a -> Maybe a -> EitherT ByteString m ()
verifyMDesc str getDesc = maybe (return ()) (verifyDesc str getDesc)

verifyDesc ::
  (MonadIO m, Cmp a) => ByteString -> IO a -> a -> EitherT ByteString m ()
verifyDesc str getDesc oldDesc = do
  newDesc <- liftIO getDesc
  case Cmp.cmp oldDesc newDesc of
    Cmp.Equals -> return ()
    Cmp.NotEquals reasons ->
      -- fail entire computation
      left $ str <> ": " <> BS8.intercalate ", " reasons

#if MIN_VERSION_base(4,8,0)
bimapEitherT' :: Functor f => (e -> e') -> (a -> a') -> EitherT e f a -> EitherT e' f a'
bimapEitherT' = bimapEitherT

annotateError :: Functor f => b -> EitherT e f a -> EitherT (e, b) f a
#else
bimapEitherT' :: Monad m => (e -> e') -> (a -> a') -> EitherT e m a -> EitherT e' m a'
bimapEitherT' f g (EitherT m) = EitherT (m >>= (return . h)) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)

annotateError :: Monad m => b -> EitherT e m a -> EitherT (e,b) m a
#endif

annotateError ann = bimapEitherT' (, ann) id

verifyInputsSummary ::
    MonadIO f => Db -> BuildTargetEnv -> TargetDesc
    -> Map FilePath (Db.FileDesc () Db.InputSummary)
    -> EitherT (ByteString, FilePath) f ()
verifyInputsSummary db BuildTargetEnv{..} TargetDesc{..} esInputsDescs = do
  forM_ (M.toList esInputsDescs) $ \(filePath, desc) -> do
    mStat <- liftIO $ Dir.getMFileStatus filePath
    annotateError filePath $
      case (mStat, desc) of
        (Nothing, Db.FileDescNonExisting ()) -> return ()
        (Just _, Db.FileDescNonExisting ()) -> left "created"
        (Nothing, Db.FileDescExisting _) -> left "deleted"
        (Just stat, Db.FileDescExisting summary) ->
          case summary of
          Db.InputMTime mtime Db.InputDesc{..}
            | mtime == Posix.modificationTimeHiRes stat -> return ()
            | otherwise -> do
              verify "mode" (return (fileModeDescOfStat stat)) idModeAccess
              verify "stat" (return (fileStatDescOfStat stat)) idStatAccess
              verify "content"
                (fileContentDescOfStat "When applying execution log (input)"
                 db filePath stat) idContentAccess
          Db.InputModeDesc modeDesc ->
            unless (fileModeDescOfStat stat == modeDesc) $ left "mode changed"
  where
    verify str getDesc mPair =
      verifyMDesc ("input(" <> str <> ")") getDesc $ snd <$> mPair

verifyOutputDescs ::
    MonadIO m => Db -> BuildTargetEnv -> TargetDesc ->
    Map FilePath (Db.FileDesc ne (POSIXTime, Db.OutputDesc)) ->
    EitherT (ByteString, FilePath) m ()
verifyOutputDescs db bte@BuildTargetEnv{..} TargetDesc{..} esOutputsDescs = do
  -- For now, we don't store the output files' content
  -- anywhere besides the actual output files, so just verify
  -- the output content is still correct
  forM_ (M.toList esOutputsDescs) $ \(filePath, outputDesc) -> do
      annotateError filePath $
        verifyFileDesc bte filePath outputDesc $
        \stat (Db.OutputDesc oldStatDesc oldMContentDesc) -> do
              verifyDesc "output(stat)" (return (fileStatDescOfStat stat)) oldStatDesc
              verifyMDesc "output(content)"
                (fileContentDescOfStat "When applying execution log (output)"
                 db filePath stat) oldMContentDesc

executionSummaryVerifyFilesState ::
  MonadIO m =>
  BuildTargetEnv -> TargetDesc -> Db.ExecutionFilesSummary ->
  EitherT (ByteString, FilePath) m ()
executionSummaryVerifyFilesState bte@BuildTargetEnv{..} TargetDesc{..} Db.ExecutionFilesSummary{..} = do
  verifyInputsSummary db bte TargetDesc{..} efsInputsDescs
  verifyOutputDescs db bte TargetDesc{..} efsOutputsDescs
  liftIO $
    replayExecutionLog bte tdTarget (M.keysSet efsInputsDescs) (M.keysSet efsOutputsDescs)
    efsStdErr efsSelfTime
  where
    db = bsDb bteBuildsome

executionLogBuildInputs ::
    BuildTargetEnv -> Parallelism.Entity -> TargetDesc ->
    Map FilePath (Db.FileDesc () Db.InputSummary) -> IO BuiltTargets
executionLogBuildInputs bte@BuildTargetEnv{..} entity TargetDesc{..} esInputsDescs = do
  -- TODO: This is good for parallelism, but bad if the set of
  -- inputs changed, as it may build stuff that's no longer
  -- required:
  speculativeSlaves <- concat <$> mapM mkInputSlaves (M.toList esInputsDescs)
  waitForSlavesWithParReleased bte entity speculativeSlaves
  where
    mkInputSlaves (inputPath, desc)
      | inputPath `S.member` hinted = return []
      | otherwise =
        slavesFor mkBte $ slaveRequest desc inputPath
    slaveRequest (Db.FileDescExisting (Db.InputModeDesc _)) = SlaveRequestDirect
    slaveRequest (Db.FileDescExisting (Db.InputMTime{})) = SlaveRequestFull
    slaveRequest (Db.FileDescNonExisting ()) = SlaveRequestDirect
    Color.Scheme{..} = Color.scheme
    hinted = S.fromList $ targetAllInputs tdTarget
    mkBte =
        bte
        { bteExplicitlyDemanded = False
        , bteSpeculative = True
        , bteReason = Db.BecauseSpeculative bteReason
        }

parentDirs :: [FilePath] -> [FilePath]
parentDirs = map FilePath.takeDirectory . filter (`notElem` ["", "/"])

buildManyWithParReleased ::
  (FilePath -> Reason) -> BuildTargetEnv -> Parallelism.Entity -> [SlaveRequest] -> IO BuiltTargets
buildManyWithParReleased mkReason bte@BuildTargetEnv{..} entity slaveRequests =
  do
    waitForSlavesWithParReleased bte entity =<< fmap concat (mapM mkSlave slaveRequests)
  where
    mkSlave req =
      slavesFor bte { bteReason = mkReason (inputFilePath req) } req
    Color.Scheme{..} = Color.scheme

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog ::
    BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> Db.ExecutionSummary ->
    IO (Maybe (Db.ExecutionFilesSummary, BuiltTargets))
findApplyExecutionLog bte@BuildTargetEnv{..} entity TargetDesc{..} executionLog = do
  eRes <- tryApplyExecutionLog bte entity TargetDesc{..} executionLog
  case eRes of
    Left (SpeculativeBuildFailure exception:_)
      | isThreadKilled exception -> return Nothing
    Left err -> do
      printStrLn btePrinter $ bsRender bteBuildsome $ mconcat $
        [ show $ length err
        , " execution log(s) of ", cTarget (show (targetOutputs tdTarget))
        , " did not match; for example because ", head $ map describeError err
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
        filteredOutputs <- fmap (S.fromList . catMaybes) $ mapM addMEffect outputs
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
  (time, stdOutputs) <-
    FSHook.timedRunCommand hook rootPath shellCmd (targetOutputs target) accessHandlers
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
    shellCmd = shellCmdVerify bte target ["HOME", "PATH"]
    Color.Scheme{..} = Color.scheme

inputAccess ::
  Db -> FilePath ->
  (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
  IO (Db.FileDesc Reason (POSIXTime, Db.InputDesc Reason))
inputAccess db path (accessTypes, mStat) =
  case mStat of
  Nothing -> do
    assertFileMTime Nothing
    return $ Db.FileDescNonExisting reason
    where
      reason = case M.elems accessTypes of
        [] -> error $ "AccessTypes empty in rcrInputs:" ++ show path
        x:_ -> x
  Just stat -> do
    assertFileMTime $ Just stat
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
  where
    assertFileMTime oldMStat =
      unless (MagicFiles.allowedUnspecifiedOutput path) $
      Meddling.assertFileMTime "When making execution log (input)" path oldMStat

makeExecutionLog ::
  Buildsome -> Target ->
  Map FilePath (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
  [FilePath] -> StdOutputs ByteString -> DiffTime -> IO Db.ExecutionLog
makeExecutionLog buildsome target inputs outputs stdOutputs selfTime = do
  inputsDescs <- M.traverseWithKey (inputAccess db) inputs
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
            else do
              chash <- fileContentDescOfStat "When making execution log (output)"
                       db outPath stat
              case chash of
                FileContentDescRegular contentHash ->
                  if BS8.null contentHash
                  then return ()
                  else do
                    let targetPath = mkTargetWithHashPath buildsome contentHash
                    -- putStrLn $ BS8.unpack ("Copying: " <> outPath <> " -> " <> targetPath)
                    alreadyExists <- FilePath.exists targetPath
                    unless alreadyExists $ do
                      removeFileOrDirectoryOrNothing targetPath
                      Dir.createDirectories $ FilePath.takeDirectory targetPath
                      Dir.copyFile outPath targetPath
                _ -> return ()
              return $ Just chash
            -- else Just <$>
            --      fileContentDescOfStat "When making execution log (output)"
            --      db outPath stat
          let mtime = Posix.modificationTimeHiRes stat
          return $ Db.FileDescExisting
            (mtime, Db.OutputDesc (fileStatDescOfStat stat) mContentDesc)
      return (outPath, fileDesc)
  return Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elCommand = targetCmds target
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = stdOutputs
    , elSelfTime = selfTime
    }
  where
    db = bsDb buildsome

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
  BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> Maybe Db.ExecutionSummary -> IO (Db.ExecutionFilesSummary, BuiltTargets)
buildTargetReal bte@BuildTargetEnv{..} entity TargetDesc{..} mOldExecutionSummary =
  Print.targetWrap btePrinter bteReason tdTarget "BUILDING" $ do
    deleteOldTargetOutputs bte tdTarget

    Print.executionCmd verbosityCommands btePrinter tdTarget

    RunCmdResults{..} <- runCmd bte entity tdTarget

    outputs <- verifyTargetSpec bte (M.keysSet rcrInputs) (M.keysSet rcrOutputs) tdTarget
    executionLog <-
      makeExecutionLog bteBuildsome tdTarget rcrInputs (S.toList outputs)
      rcrStdOutputs rcrSelfTime

    curTime <- getPOSIXTime
    writeIRef (Db.executionLog tdTarget (bsDb bteBuildsome)) executionLog
    let executionFilesSummary = Db.summarizeExecutionLog executionLog
        executionSummary =
          case mOldExecutionSummary of
            Nothing -> Db.ExecutionSummary { esFilesSummary = NonEmptyList.singleton (curTime, executionFilesSummary) }
            Just es -> Db.appendSummary curTime executionFilesSummary es

    writeIRef (Db.executionSummary tdTarget (bsDb bteBuildsome)) executionSummary

    Print.targetTiming btePrinter "now" rcrSelfTime
    return (executionFilesSummary, rcrBuiltTargets)
  where
    verbosityCommands = Opts.verbosityCommands verbosity
    verbosity = optVerbosity (bsOpts bteBuildsome)

buildTarget :: BuildTargetEnv -> Parallelism.Entity -> TargetDesc -> IO Stats
buildTarget bte@BuildTargetEnv{..} entity TargetDesc{..} =
  maybeRedirectExceptions bte TargetDesc{..} $ do
    (explicitPathsBuilt, hintedBuiltTargets) <- buildTargetHints bte entity tdTarget
    case explicitPathsBuilt of
      ExplicitPathsNotBuilt ->
        -- Failed to build our hints when allowed, just leave with collected stats
        return $ builtStats hintedBuiltTargets
      ExplicitPathsBuilt -> do
        let buildReal x = (,) Stats.BuiltNow <$> buildTargetReal bte entity TargetDesc{..} x
        (whenBuilt, (Db.ExecutionFilesSummary{..}, builtTargets)) <-
          readIRef (Db.executionSummary tdTarget (bsDb bteBuildsome)) >>= \case
            Nothing -> buildReal Nothing
            Just executionSummary -> do
              findApplyExecutionLog bte entity TargetDesc{..} executionSummary >>= \case
                Just res -> return (Stats.FromCache, res)
                Nothing -> buildReal $ Just executionSummary
        let BuiltTargets deps stats = hintedBuiltTargets <> builtTargets
        return $ stats <>
          Stats
          { Stats.ofTarget =
               M.singleton tdRep Stats.TargetStats
               { tsWhen = whenBuilt
               , tsTime = efsSelfTime
               , tsDirectDeps = deps
               }
          , Stats.stdErr =
            if mempty /= efsStdErr
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

data FileBuildRule
  = NoBuildRule
  | ValidBuildRule
  | InvalidPatternBuildRule {- transitively missing inputs -}
  deriving (Eq)
getFileBuildRule :: Set FilePath -> BuildMaps -> FilePath -> IO FileBuildRule
getFileBuildRule registeredOutputs buildMaps = go
  where
    go path =
      case BuildMaps.find buildMaps path of
      Nothing -> return NoBuildRule
      Just (_rep, BuildMaps.TargetSimple, _) -> return ValidBuildRule
      Just (_rep, BuildMaps.TargetPattern, tgt) ->
        do
          inputsCanExist <- and <$> mapM fileCanExist (targetAllInputs tgt)
          return $
            if inputsCanExist
            then ValidBuildRule
            else InvalidPatternBuildRule
    fileCanExist path = do
      fileBuildRule <- go path
      case fileBuildRule of
        InvalidPatternBuildRule -> return False
        ValidBuildRule -> return True
        NoBuildRule
          | path `S.member` registeredOutputs -> return False -- a has-been
          | otherwise -> FilePath.exists path

deleteRemovedOutputs :: Printer -> Db -> BuildMaps -> IO ()
deleteRemovedOutputs printer db buildMaps = do
  -- No need for IORef atomicity here, this happens strictly before
  -- anything else, without parallelism
  oldRegisteredOutputs <- readIORef (Db.registeredOutputsRef db)
  (newRegisteredOutputs, toDelete) <-
    LibSet.partitionA
    (fmap (== ValidBuildRule) .
     getFileBuildRule oldRegisteredOutputs buildMaps)
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
      cmd = targetCmds target

shellCmdVerify ::
  BuildTargetEnv -> Target -> [String] -> Process.Env -> IO (StdOutputs ByteString)
shellCmdVerify BuildTargetEnv{..} target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand (BS8.unpack (targetCmds target))) inheritEnvs newEnvs
  let stdOutputs = StdOutputs stdout stderr
  case exitCode of
    ExitFailure {} ->
      E.throwIO $ TargetCommandFailed (bsRender bteBuildsome) target exitCode stdOutputs
    _ -> return ()
  Print.targetStdOutputs btePrinter target stdOutputs
  return stdOutputs

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
    deleteRemovedOutputs printer db buildMaps

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
        , bsBuildsomePath = buildDbFilename makefilePath
        , bsPhoniesSet = S.fromList . map snd $ makefilePhonies makefile
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
      (cleanContentCacheDir buildsome >> body buildsome)
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
