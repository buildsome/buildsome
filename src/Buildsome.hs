{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Buildsome
  ( Buildsome, with
  , clean, want
  ) where

import Buildsome.Db (Db, IRef(..), Reason)
import Buildsome.FileContentDescCache (fileContentDescOfStat)
import Buildsome.Meddling (assertSameMTime)
import Buildsome.Opts (Opt(..))
import Buildsome.Print (targetShow)
import Control.Applicative ((<$>), Applicative(..))
import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import Control.Exception.Async (handleSync)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lib.AnnotatedException (annotateException)
import Lib.BuildId (BuildId)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.ColorText (ColorText, renderStr)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing)
import Lib.Exception (finally)
import Lib.FSHook (FSHook, OutputBehavior(..), HasEffect(..))
import Lib.FileDesc (fileModeDescOfStat, fileStatDescOfStat)
import Lib.FilePath (FilePath, (</>), (<.>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import Lib.Makefile (Makefile(..), TargetType(..), Target, targetAllInputs)
import Lib.Parallelism (Parallelism)
import Lib.Parsec (showPos)
import Lib.Printer (Printer, printStrLn)
import Lib.Show (show)
import Lib.ShowBytes (showBytes)
import Lib.Slave (Slave)
import Lib.StdOutputs (StdOutputs(..))
import Lib.TimeIt (timeIt)
import Prelude hiding (FilePath, show)
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(..))
import Text.Parsec (SourcePos)
import qualified Buildsome.Clean as Clean
import qualified Buildsome.Color as Color
import qualified Buildsome.Db as Db
import qualified Buildsome.MagicFiles as MagicFiles
import qualified Buildsome.Opts as Opts
import qualified Buildsome.Print as Print
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Lib.BuildId as BuildId
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.ColorText as ColorText
import qualified Lib.Directory as Dir
import qualified Lib.FSHook as FSHook
import qualified Lib.FilePath as FilePath
import qualified Lib.Fresh as Fresh
import qualified Lib.Parallelism as Parallelism
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Slave as Slave
import qualified Lib.Timeout as Timeout
import qualified Prelude
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

data Explicitness = Explicit | Implicit
  deriving (Eq)

type Parents = [(TargetRep, Reason)]

data Buildsome = Buildsome
  { -- static:
    bsOpts :: Opt
  , bsMakefile :: Makefile
  , bsBuildId :: BuildId
  , bsRootPath :: FilePath
  , bsBuildMaps :: BuildMaps
    -- dynamic:
  , bsDb :: Db
  , bsFsHook :: FSHook
  , bsSlaveByTargetRep :: IORef (Map TargetRep (MVar Slave))
  , bsParallelism :: Parallelism
  , bsFreshPrinterIds :: Fresh Printer.Id
  , bsFastKillBuild :: E.SomeException -> IO ()
  }

cancelAllSlaves :: Buildsome -> IO ()
cancelAllSlaves bs = go 0
  where
    timeoutWarning time slave =
      Timeout.warning time $
      concat ["Slave ", Slave.str slave, " did not cancel in ", show time, "!"]
    go alreadyCancelled = do
      curSlaveMap <- readIORef $ bsSlaveByTargetRep bs
      slaves <-
        forM (M.toList curSlaveMap) $
        \(targetRep, mvar) ->
        Timeout.warning (Timeout.millis 100)
        (renderStr
         ("Slave MVar for " <> targetShow targetRep <> " not populated in 100 millis!")) $
        readMVar mvar
      let count = length slaves
      unless (alreadyCancelled >= count) $ do
        forM_ slaves $ \slave -> timeoutWarning (Timeout.millis 100) slave $
          Slave.cancel slave
        forM_ slaves $ \slave -> timeoutWarning (Timeout.seconds 2) slave $
          Slave.waitCatch slave
        -- Make sure to cancel any potential new slaves that were
        -- created during cancellation
        go count

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = FilePath.takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      gitIgnoreBasePath = dir </> gitignoreBaseName
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  putStrLn "Updating .gitignore"
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
  }

mkSlavesForPaths :: BuildTargetEnv -> Explicitness -> [FilePath] -> IO [Slave]
mkSlavesForPaths bte explicitness = fmap concat . mapM (mkSlaves bte explicitness)

want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO Slave.Stats
want printer buildsome reason paths = do
  printStrLn printer $
    "Building: " <> ColorText.intercalate ", " (map targetShow paths)
  (buildTime, slaveStats) <- timeIt $
    fmap mconcat . mapM Slave.wait =<<
    mkSlavesForPaths BuildTargetEnv
    { bteBuildsome = buildsome
    , btePrinter = printer
    , bteReason = reason
    , bteParents = []
    } Explicit paths
  printStrLn printer $ mconcat
    [ Color.success "Build Successful", ": "
    , Color.timing (show buildTime <> " seconds"), " total." ]
  return slaveStats

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- FilePath.exists path
  unless doesExist $ E.throwIO err

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

data MissingRule = MissingRule FilePath Reason deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule path reason) =
    renderStr $ Color.error $ mconcat
    ["ERROR: No rule to build ", targetShow path, " (", fromBytestring8 reason, ")"]

data TargetNotCreated = TargetNotCreated FilePath Target deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated path target) =
    renderStr $ Color.error $ mconcat
    [ (fromString . showPos . targetPos) target
    , targetShow path
    , " explicitly demanded but was not created by its target rule" ]

mkSlavesDirectAccess :: BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlavesDirectAccess bte@BuildTargetEnv{..} explicitness path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $ assertExists path $ MissingRule path bteReason
    return []
  Just tgt@(_, target) -> do
    slave <- getSlaveForTarget bte tgt
    (: []) <$> case explicitness of
      Implicit -> return slave
      Explicit -> verifyFileGetsCreated slave
      where
        verifyFileGetsCreated slave
          | path `elem` makefilePhonies (bsMakefile bteBuildsome) = return slave
          | otherwise =
            Slave.wrap (<* assertExists path (TargetNotCreated path target)) slave

makeChildSlaves :: BuildTargetEnv -> FilePath -> IO [Slave]
makeChildSlaves bte@BuildTargetEnv{..} path
  | not (null childPatterns) =
    fail "UNSUPPORTED: Read directory on directory with patterns"
  | otherwise =
    traverse (getSlaveForTarget bte) $
    filter (not . isPhony . snd) childTargets
  where
    isPhony = all (`S.member` phoniesSet bteBuildsome) . targetOutputs
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps bteBuildsome) path

mkSlavesForAccessType ::
  FSHook.AccessType -> BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlavesForAccessType FSHook.AccessTypeFull = mkSlaves
mkSlavesForAccessType FSHook.AccessTypeModeOnly = mkSlavesDirectAccess
mkSlavesForAccessType FSHook.AccessTypeStat =
  -- This is a (necessary) hack! See KNOWN_ISSUES: "stat of directories"
  mkSlavesDirectAccess

mkSlaves :: BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
mkSlaves bte@BuildTargetEnv{..} explicitness path
  | FilePath.isAbsolute path = return [] -- Only project-relative paths may have output rules
  | otherwise = do
  slaves <- mkSlavesDirectAccess bte explicitness path
  childs <- makeChildSlaves bte { bteReason = bteReason <> " (Container directory)" } path
  return $ slaves ++ childs

-- e.g: .pyc files
handleLegalUnspecifiedOutputs :: Buildsome -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs buildsome target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ Print.warn (targetPos target) $
    mconcat ["Leaked unspecified outputs: ", show paths, ", ", actionDesc]
  action
  where
    (actionDesc, action) =
      case optDeleteUnspecifiedOutputs (bsOpts buildsome) of
      Opts.DeleteUnspecifiedOutputs -> ("deleting", mapM_ removeFileOrDirectoryOrNothing paths)
      Opts.DontDeleteUnspecifiedOutputs -> ("keeping", registerLeakedOutputs buildsome (S.fromList paths))

data IllegalUnspecifiedOutputs = IllegalUnspecifiedOutputs Target [FilePath] deriving (Typeable)
instance E.Exception IllegalUnspecifiedOutputs
instance Show IllegalUnspecifiedOutputs where
  show (IllegalUnspecifiedOutputs target illegalOutputs) =
    renderStr $ Color.error $ mconcat
    [ "Target: ", targetShow (targetOutputs target)
    , " wrote to unspecified output files: ", show illegalOutputs ]

-- Verify output of whole of slave/execution log
verifyTargetSpec :: Buildsome -> Set FilePath -> Set FilePath -> Target -> IO ()
verifyTargetSpec buildsome inputs outputs target = do
  verifyTargetInputs buildsome inputs target
  verifyTargetOutputs buildsome outputs target

phoniesSet :: Buildsome -> Set FilePath
phoniesSet = S.fromList . makefilePhonies . bsMakefile

verifyTargetInputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetInputs buildsome inputs target
  | all (`S.member` phoniesSet buildsome)
    (targetOutputs target) = return () -- Phony target doesn't need real inputs
  | otherwise =
    warnOverSpecified "inputs" "" (S.fromList (targetAllInputs target))
    (inputs `S.union` phoniesSet buildsome) (targetPos target)

verifyTargetOutputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs buildsome outputs target = do
  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs buildsome target =<<
    filterM FilePath.exists unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM FilePath.exists illegalOutputs
  unless (null existingIllegalOutputs) $ do
    Print.posMessage (targetPos target) $ Color.error $
      "Illegal output files created: " <> show existingIllegalOutputs

    Print.warn (targetPos target) $ "leaving leaked unspecified output effects" -- Need to make sure we only record actual outputs, and not *attempted* outputs before we delete this
    -- mapM_ removeFileOrDirectory existingIllegalOutputs

    E.throwIO $ IllegalUnspecifiedOutputs target existingIllegalOutputs
  warnOverSpecified "outputs" " (consider adding a .PHONY declaration)" specified (outputs `S.union` phoniesSet buildsome) (targetPos target)
  where
    (unspecifiedOutputs, illegalOutputs) =
      partition MagicFiles.allowedUnspecifiedOutput allUnspecified
    allUnspecified = S.toList $ outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

warnOverSpecified ::
  ColorText -> ColorText -> Set FilePath -> Set FilePath -> SourcePos -> IO ()
warnOverSpecified str suffix specified used pos =
  unless (S.null unused) $
  Print.warn pos $ mconcat
  ["Over-specified ", str, ": ", targetShow (S.toList unused), suffix]
  where
    unused = specified `S.difference` used

-- Already verified that the execution log is a match
applyExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  StdOutputs ByteString -> DiffTime -> IO ()
applyExecutionLog BuildTargetEnv{..} target inputs outputs stdOutputs selfTime =
  Print.targetWrap btePrinter bteReason target "REPLAY" $ do
    Print.cmd btePrinter target
    Print.targetStdOutputs target stdOutputs
    verifyTargetSpec bteBuildsome inputs outputs target
    printStrLn btePrinter $ ColorText.render $
      "Build (originally) took " <> Color.timing (show selfTime <> " seconds")

waitForSlaves :: Printer -> Parallelism.Cell -> Buildsome -> [Slave] -> IO Slave.Stats
waitForSlaves _ _ _ [] = return mempty
waitForSlaves _printer parCell buildsome slaves =
  Parallelism.withReleased parCell (bsParallelism buildsome) $
  mconcat <$> mapM Slave.wait slaves

verifyFileDesc ::
  (IsString str, Monoid str, MonadIO m) =>
  str -> FilePath -> Db.FileDesc ne desc ->
  (Posix.FileStatus -> desc -> EitherT (str, FilePath) m ()) ->
  EitherT (str, FilePath) m ()
verifyFileDesc str filePath fileDesc existingVerify = do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case (mStat, fileDesc) of
    (Nothing, Db.FileDescNonExisting _) -> return ()
    (Just stat, Db.FileDescExisting desc) -> existingVerify stat desc
    (Just _, Db.FileDescNonExisting _)  -> left (str <> " file did not exist, now exists", filePath)
    (Nothing, Db.FileDescExisting {}) -> left (str <> " file was deleted", filePath)

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Cell ->
  TargetRep -> Target -> Db.ExecutionLog ->
  IO (Either (ByteString, FilePath) Slave.Stats)
tryApplyExecutionLog bte@BuildTargetEnv{..} parCell targetRep target el@Db.ExecutionLog {..} = do
  nestedSlaveStats <- executionLogWaitForInputs bte parCell target el
  runEitherT $ do
    forM_ (M.toList elInputsDescs) $ \(filePath, desc) ->
      verifyFileDesc "input" filePath desc $ \stat (Db.InputDesc mModeAccess mStatAccess mContentAccess) -> do
        let verify str getDesc mPair = verifyMDesc ("input(" <> str <> ")") filePath getDesc $ snd <$> mPair
        verify "mode" (return (fileModeDescOfStat stat)) mModeAccess
        verify "stat" (return (fileStatDescOfStat stat)) mStatAccess
        verify "content" (fileContentDescOfStat db filePath stat) mContentAccess
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    forM_ (M.toList elOutputsDescs) $ \(filePath, outputDesc) -> do
      verifyFileDesc "output" filePath outputDesc $ \stat (Db.OutputDesc oldStatDesc oldMContentDesc) -> do
        verifyDesc  "output(stat)"    filePath (return (fileStatDescOfStat stat)) oldStatDesc
        verifyMDesc "output(content)" filePath (fileContentDescOfStat db filePath stat) oldMContentDesc
    liftIO $
      applyExecutionLog bte target
      (M.keysSet elInputsDescs) (M.keysSet elOutputsDescs)
      elStdoutputs elSelfTime
    let selfStats = Slave.Stats $ M.singleton targetRep (Slave.FromCache, elSelfTime)
    return $ mappend selfStats nestedSlaveStats
  where
    db = bsDb bteBuildsome
    verifyMDesc _   _        _       Nothing        = return ()
    verifyMDesc str filePath getDesc (Just oldDesc) =
      verifyDesc str filePath getDesc oldDesc

    verifyDesc str filePath getDesc oldDesc = do
      newDesc <- liftIO $ getDesc
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation

executionLogWaitForInputs :: BuildTargetEnv -> Parallelism.Cell -> Target -> Db.ExecutionLog -> IO Slave.Stats
executionLogWaitForInputs bte@BuildTargetEnv{..} parCell target Db.ExecutionLog {..} = do
  -- TODO: This is good for parallelism, but bad if the set of
  -- inputs changed, as it may build stuff that's no longer
  -- required:
  speculativeSlaves <- fmap concat $ mapM mkInputSlave (M.toList elInputsDescs)

  let hintReason = ColorText.render $ "Hint from " <> (targetShow . targetOutputs) target
  hintedSlaves <-
    mkSlavesForPaths bte { bteReason = hintReason } Explicit $ targetAllInputs target

  targetParentsStats <- buildParentDirectories bte parCell Explicit $ targetOutputs target

  let allSlaves = speculativeSlaves ++ hintedSlaves
  mappend targetParentsStats <$>
    waitForSlaves btePrinter parCell bteBuildsome allSlaves
  where
    hinted = S.fromList $ targetAllInputs target
    mkInputSlave (inputPath, desc)
      | inputPath `S.member` hinted = return []
      | otherwise = mkInputSlaveFor desc Implicit inputPath
    mkInputSlaveFor (Db.FileDescNonExisting depReason) =
      mkSlavesDirectAccess bte { bteReason = depReason }
    mkInputSlaveFor (Db.FileDescExisting inputDesc) =
      case inputDesc of
      Db.InputDesc { Db.idContentAccess = Just (depReason, _) } -> mkSlaves bte { bteReason = depReason }
      Db.InputDesc { Db.idStatAccess = Just (depReason, _) } -> mkSlavesDirectAccess bte { bteReason = depReason }
      Db.InputDesc { Db.idModeAccess = Just (depReason, _) } -> mkSlavesDirectAccess bte { bteReason = depReason }
      Db.InputDesc Nothing Nothing Nothing -> const $ const $ return []

buildParentDirectories :: BuildTargetEnv -> Parallelism.Cell -> Explicitness -> [FilePath] -> IO Slave.Stats
buildParentDirectories bte@BuildTargetEnv{..} parCell explicitness =
  waitForSlaves btePrinter parCell bteBuildsome . concat <=<
  mapM mkParentSlaves . filter (`notElem` ["", "/"])
  where
    mkParentSlaves path =
      mkSlavesDirectAccess bte
      { bteReason =
        ColorText.render $ "Container directory of: " <> targetShow path
      } explicitness (FilePath.takeDirectory path)

-- TODO: Remember the order of input files' access so can iterate here
-- in order
findApplyExecutionLog :: BuildTargetEnv -> Parallelism.Cell -> TargetRep -> Target -> IO (Maybe Slave.Stats)
findApplyExecutionLog bte@BuildTargetEnv{..} parCell targetRep target = do
  mExecutionLog <- readIRef $ Db.executionLog target $ bsDb bteBuildsome
  case mExecutionLog of
    Nothing -> -- No previous execution log
      return Nothing
    Just executionLog -> do
      res <- tryApplyExecutionLog bte parCell targetRep target executionLog
      case res of
        Left (str, filePath) -> do
          printStrLn btePrinter $ ColorText.render $ mconcat
            [ "Execution log of ", targetShow (targetOutputs target)
            , " did not match because ", fromBytestring8 str, ": "
            , Color.path (show filePath)
            ]
          return Nothing
        Right stats -> return (Just stats)

showParents :: Parents -> ByteString
showParents = ColorText.render . mconcat . map showParent
  where
    showParent (targetRep, reason) =
      mconcat ["\n-> ", targetShow targetRep, " (", fromBytestring8 reason, ")"]

data TargetDependencyLoop = TargetDependencyLoop Parents deriving (Typeable)
instance E.Exception TargetDependencyLoop
instance Show TargetDependencyLoop where
  show (TargetDependencyLoop parents) =
    renderStr $ Color.error $ fromBytestring8 $ "Target loop: " <> showParents parents

data PanicError = PanicError String deriving (Typeable)
instance E.Exception PanicError
instance Show PanicError where
  show (PanicError msg) =
    renderStr $ Color.error $ fromString $ "PANIC: " ++ msg

panic :: String -> IO a
panic msg = do
  IO.hPutStrLn IO.stderr msg
  E.throwIO $ PanicError msg

-- Find existing slave for target, or spawn a new one
getSlaveForTarget :: BuildTargetEnv -> (TargetRep, Target) -> IO Slave
getSlaveForTarget bte@BuildTargetEnv{..} (targetRep, target)
  | any ((== targetRep) . fst) bteParents = E.throwIO $ TargetDependencyLoop newParents
  | otherwise = do
    newSlaveMVar <- newEmptyMVar
    E.mask $ \restoreMask -> join $
      atomicModifyIORef (bsSlaveByTargetRep bteBuildsome) $
      \oldSlaveMap ->
      -- TODO: Use a faster method to lookup&insert at the same time
      case M.lookup targetRep oldSlaveMap of
      Just slaveMVar -> (oldSlaveMap, readMVar slaveMVar)
      Nothing ->
        ( M.insert targetRep newSlaveMVar oldSlaveMap
        , mkSlave newSlaveMVar $ \printer allocParCell ->
          allocParCell $ \parCell -> restoreMask $ do
            let newBte = bte { bteParents = newParents, btePrinter = printer }
            buildTarget newBte parCell targetRep target
        )
    where
      annotate =
        annotateException $ renderStr $
        "build failure of " <> targetShow (targetOutputs target) <> ":\n"
      newParents = (targetRep, bteReason) : bteParents
      panicHandler e@E.SomeException {} = panic $ "FAILED during making of slave: " ++ show e
      mkSlave mvar action = do
        slave <-
          E.handle panicHandler $ do
            allocParCell <- Parallelism.startAlloc $ bsParallelism bteBuildsome
            depPrinterId <- Fresh.next $ bsFreshPrinterIds bteBuildsome
            depPrinter <- Printer.newFrom btePrinter depPrinterId
            Slave.new depPrinterId (targetOutputs target) $ annotate $ action depPrinter allocParCell
        putMVar mvar slave
        return slave

data UnregisteredOutputFileExists = UnregisteredOutputFileExists FilePath deriving (Typeable)
instance E.Exception UnregisteredOutputFileExists
instance Show UnregisteredOutputFileExists where
  show (UnregisteredOutputFileExists path) =
    renderStr $ Color.error $ mconcat
    [ targetShow path, " specified as output but exists as a file that "
    , "was not created by buildsome (use --overwrite to go ahead "
    , "anyway)" ]

removeOldUnregisteredOutput :: Printer -> Buildsome -> FilePath -> IO ()
removeOldUnregisteredOutput printer buildsome path =
  case optOverwriteUnregisteredOutputs (bsOpts buildsome) of
  Opts.DontOverwriteUnregisteredOutputs -> E.throwIO $ UnregisteredOutputFileExists path
  Opts.OverwriteUnregisteredOutputs -> do
    printStrLn printer $ ColorText.render $
      "Overwriting " <> targetShow path <> " (due to --overwrite)"
    removeFileOrDirectory path

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
  , rcrSlaveStats :: Slave.Stats
  }

outputHasEffect :: FSHook.Output -> IO Bool
outputHasEffect (FSHook.Output behavior path) = do
  outputExists <- FilePath.exists path
  return $
    case (outputExists, behavior) of
    (True, OutputBehavior { behaviorWhenFileDoesExist = HasEffect }) -> True
    (False, OutputBehavior { behaviorWhenFileDoesNotExist = HasEffect }) -> True
    _ -> False

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

recordOutputs :: Buildsome -> IORef (Map FilePath Reason) -> Reason -> Set FilePath -> Set FilePath -> IO ()
recordOutputs buildsome outputsRef accessDoc targetOutputsSet paths = do
  atomicModifyIORef'_ outputsRef $ M.union $
    M.fromList [(path, accessDoc) | path <- S.toList paths]
  registerOutputs buildsome $ paths `S.intersection` targetOutputsSet

runCmd :: BuildTargetEnv -> Parallelism.Cell -> Target -> IO RunCmdResults
runCmd bte@BuildTargetEnv{..} parCell target = do
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef M.empty
  slaveStatsRef <- newIORef mempty
  let
    mappendSlaveStats stats = atomicModifyIORef_ slaveStatsRef (mappend stats)
    makeInputs accessDoc inputs outputs = do
      let allPaths = map FSHook.inputPath inputs ++ map FSHook.outputPath outputs
      parentSlaveStats <- buildParentDirectories bte parCell Implicit allPaths
      slaves <-
        fmap concat $ forM inputs $ \(FSHook.Input accessType path) ->
        mkSlavesForAccessType accessType bte { bteReason = accessDoc } Implicit path
      mappendSlaveStats . mappend parentSlaveStats =<< waitForSlaves btePrinter parCell bteBuildsome slaves
    outputIgnored = MagicFiles.outputIgnored . FSHook.outputPath
    inputIgnored recordedOutputs (FSHook.Input _ path) =
      MagicFiles.inputIgnored path ||
      path `M.member` recordedOutputs ||
      path `S.member` targetOutputsSet
    fsAccessHandler isDelayed accessDoc rawInputs rawOutputs = do
      recordedOutputs <- readIORef outputsRef
      let outputs = filter (not . outputIgnored) rawOutputs
          inputs = filter (not . inputIgnored recordedOutputs) rawInputs
      filteredOutputs <-
        case isDelayed of
        FSHook.NotDelayed ->
          -- We'd like to filter those with no effect, but due to
          -- undelayed fs access, we really can't
          return outputs
        FSHook.Delayed -> do
          makeInputs accessDoc inputs outputs
          filterM outputHasEffect outputs
      -- After makeInputs, we know the current FS state will determine
      -- output's actual behavior.
      recordOutputs bteBuildsome outputsRef accessDoc targetOutputsSet $
        S.fromList $ map FSHook.outputPath filteredOutputs
      mapM_ (recordInput inputsRef accessDoc) $
        filter ((`M.notMember` recordedOutputs) . FSHook.inputPath) inputs
  (time, stdOutputs) <-
    FSHook.timedRunCommand (bsFsHook bteBuildsome) (bsRootPath bteBuildsome)
    (shellCmdVerify target ["HOME", "PATH"])
    (ColorText.render (targetShow (targetOutputs target)))
    fsAccessHandler
  inputs <- readIORef inputsRef
  outputs <- readIORef outputsRef
  slaveStats <- readIORef slaveStatsRef
  return RunCmdResults
    { rcrStdOutputs = stdOutputs
    , rcrSelfTime = realToFrac time
    , rcrInputs =
      -- This is because we don't serialize input/output access
      -- (especially when undelayed!) to the same file. So we don't
      -- really have a correct input stat/desc here for such inputs. However, outputs should always considered as mode-only inputs.
      inputs `M.difference` outputs
    , rcrOutputs = outputs
    , rcrSlaveStats = slaveStats
    }
  where
    targetOutputsSet = S.fromList $ targetOutputs target

saveExecutionLog :: Buildsome -> Target -> RunCmdResults -> IO ()
saveExecutionLog buildsome target RunCmdResults{..} = do
  inputsDescs <- M.traverseWithKey inputAccess rcrInputs
  outputDescPairs <-
    forM (M.keys rcrOutputs) $ \outPath -> do
      mStat <- Dir.getMFileStatus outPath
      fileDesc <-
        case mStat of
        Nothing -> return $ Db.FileDescNonExisting ()
        Just stat -> do
          mContentDesc <-
            if Posix.isDirectory stat
            then return Nothing
            else Just <$> fileContentDescOfStat db outPath stat
          return $ Db.FileDescExisting $ Db.OutputDesc (fileStatDescOfStat stat) mContentDesc
      return (outPath, fileDesc)
  writeIRef (Db.executionLog target (bsDb buildsome)) Db.ExecutionLog
    { elBuildId = bsBuildId buildsome
    , elInputsDescs = inputsDescs
    , elOutputsDescs = M.fromList outputDescPairs
    , elStdoutputs = rcrStdOutputs
    , elSelfTime = rcrSelfTime
    }
  where
    db = bsDb buildsome
    inputAccess ::
      FilePath ->
      (Map FSHook.AccessType Reason, Maybe Posix.FileStatus) ->
      IO (Db.FileDesc Reason Db.InputDesc)
    inputAccess path (accessTypes, Nothing) = do
      let reason =
            case M.elems accessTypes of
            [] -> error $ "AccessTypes empty in rcrInputs:" ++ show path
            x:_ -> x
      assertSameMTime path Nothing
      return $ Db.FileDescNonExisting reason
    inputAccess path (accessTypes, Just stat) = do
      assertSameMTime path (Just stat)
      let
        addDesc accessType getDesc = do
          desc <- getDesc
          return $ flip (,) desc <$> M.lookup accessType accessTypes
      modeAccess <-
        addDesc FSHook.AccessTypeModeOnly $ return $ fileModeDescOfStat stat
      statAccess <-
        addDesc FSHook.AccessTypeStat $ return $ fileStatDescOfStat stat
      contentAccess <-
        addDesc FSHook.AccessTypeFull $ fileContentDescOfStat db path stat
      return $ Db.FileDescExisting Db.InputDesc
        { Db.idModeAccess = modeAccess
        , Db.idStatAccess = statAccess
        , Db.idContentAccess = contentAccess
        }

redirectExceptions :: Buildsome -> IO a -> IO a
redirectExceptions buildsome =
  handleSync $ \e@E.SomeException {} -> do
    bsFastKillBuild buildsome e
    E.throwIO e

buildTarget :: BuildTargetEnv -> Parallelism.Cell -> TargetRep -> Target -> IO Slave.Stats
buildTarget bte@BuildTargetEnv{..} parCell targetRep target =
  redirectExceptions bteBuildsome $ do
    mSlaveStats <- findApplyExecutionLog bte parCell targetRep target
    case mSlaveStats of
      Just slaveStats -> return slaveStats
      Nothing -> Print.targetWrap btePrinter bteReason target "BUILDING" $ do
        targetParentsStats <- buildParentDirectories bte parCell Explicit $ targetOutputs target

        registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb bteBuildsome
        mapM_ (removeOldOutput btePrinter bteBuildsome registeredOutputs) $ targetOutputs target

        slaves <-
          mkSlavesForPaths bte
            { bteReason = ColorText.render $ "Hint from " <> targetShow (targetOutputs target)
            } Explicit $ targetAllInputs target
        hintedStats <- waitForSlaves btePrinter parCell bteBuildsome slaves

        Print.cmd btePrinter target

        rcr@RunCmdResults{..} <- runCmd bte parCell target

        verifyTargetSpec bteBuildsome (M.keysSet rcrInputs) (M.keysSet rcrOutputs) target
        saveExecutionLog bteBuildsome target rcr

        printStrLn btePrinter $
          "Build (now) took " <> Color.timing (show rcrSelfTime <> " seconds")
        let selfStats = Slave.Stats $ M.singleton targetRep (Slave.BuiltNow, rcrSelfTime)
        return $ mconcat [selfStats, targetParentsStats, hintedStats, rcrSlaveStats]

registerDbList :: Ord a => (Db -> IORef (Set a)) -> Buildsome -> Set a -> IO ()
registerDbList mkIORef buildsome newItems =
  atomicModifyIORef'_ (mkIORef (bsDb buildsome)) (newItems <>)

registerOutputs :: Buildsome -> Set FilePath -> IO ()
registerOutputs = registerDbList Db.registeredOutputsRef

registerLeakedOutputs :: Buildsome -> Set FilePath -> IO ()
registerLeakedOutputs = registerDbList Db.leakedOutputsRef

deleteRemovedOutputs :: Buildsome -> IO ()
deleteRemovedOutputs buildsome = do
  toDelete <-
    atomicModifyIORef' (Db.registeredOutputsRef (bsDb buildsome)) $
    S.partition (isJust . BuildMaps.find (bsBuildMaps buildsome))
  forM_ (S.toList toDelete) $ \path -> do
    ColorText.putStrLn $ "Removing old output: " <> Color.path (show path)
    removeFileOrDirectoryOrNothing path

data TargetCommandFailed = TargetCommandFailed Target ExitCode (StdOutputs ByteString) deriving (Typeable)
instance E.Exception TargetCommandFailed
instance Show TargetCommandFailed where
  show (TargetCommandFailed target exitCode stdOutputs) =
    renderStr $ Color.error $ mconcat
    [ fromBytestring8 (Print.delimitMultiline cmd), " failed: ", show exitCode
    , fromMaybe "" $ Print.outputsStr " Outputs:" stdOutputs ]
    where
      cmd = targetCmds target

shellCmdVerify :: Target -> [String] -> Process.Env -> IO (StdOutputs ByteString)
shellCmdVerify target inheritEnvs newEnvs = do
  (exitCode, stdout, stderr) <-
    Process.getOutputs (ShellCommand (BS8.unpack (targetCmds target))) inheritEnvs newEnvs
  let stdOutputs = StdOutputs stdout stderr
  case exitCode of
    ExitFailure {} -> E.throwIO $ TargetCommandFailed target exitCode stdOutputs
    _ -> return ()
  Print.targetStdOutputs target stdOutputs
  return stdOutputs

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

disallowExceptions :: IO a -> String -> IO a
disallowExceptions act prefix = act `E.catch` \e@E.SomeException {} -> do
  putStrLn $ prefix ++ show e
  E.throwIO e

mkFastKillBuild :: Opts.KeepGoing -> IO (E.SomeException -> IO (), IO ())
mkFastKillBuild Opts.KeepGoing = return (const (return ()), return ())
mkFastKillBuild Opts.DieQuickly = do
  tid <- myThreadId
  killActionMVar <- newMVar (E.throwTo tid)
  let fastKillBuild e =
        modifyMVar killActionMVar $ \killAction -> do
          killAction e
          return (const (return ()), ())
      disableFastKillBuild = E.uninterruptibleMask_ $ void $ swapMVar killActionMVar (const (return ()))
  return (fastKillBuild, disableFastKillBuild)

with :: FilePath -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
with makefilePath makefile opt@Opt{..} body = do
  ldPreloadPath <- FSHook.getLdPreloadPath optFsOverrideLdPreloadPath
  FSHook.with ldPreloadPath $ \fsHook ->
    Db.with (buildDbFilename makefilePath) $ \db -> do
      slaveMapByTargetRep <- newIORef M.empty
      parallelism <- Parallelism.new $ fromMaybe 1 optParallelism
      freshPrinterIds <- Fresh.new 1
      buildId <- BuildId.new
      rootPath <- FilePath.canonicalizePath $ FilePath.takeDirectory makefilePath
      (fastKillBuild, disableFastKillBuild) <- mkFastKillBuild optKeepGoing
      let
        buildsome =
          Buildsome
          { bsOpts = opt
          , bsMakefile = makefile
          , bsBuildId = buildId
          , bsRootPath = rootPath
          , bsBuildMaps = BuildMaps.make makefile
          , bsDb = db
          , bsFsHook = fsHook
          , bsSlaveByTargetRep = slaveMapByTargetRep
          , bsParallelism = parallelism
          , bsFreshPrinterIds = freshPrinterIds
          , bsFastKillBuild = fastKillBuild
          }
      deleteRemovedOutputs buildsome
      body buildsome
        -- We must not leak running slaves as we're not allowed to
        -- access fsHook, db, etc after leaving here:
        `E.onException` putStrLn "Shutting down"
        `finally` disableFastKillBuild
        `finally` (cancelAllSlaves buildsome `disallowExceptions` "BUG: Exception thrown at cancelAllSlaves: ")
        -- Must update gitIgnore after all the slaves finished updating
        -- the registered output lists:
        `finally` maybeUpdateGitIgnore buildsome
  where
    maybeUpdateGitIgnore buildsome =
      case optUpdateGitIgnore of
      Opts.UpdateGitIgnore -> updateGitIgnore buildsome makefilePath
      Opts.DontUpdateGitIgnore -> return ()

clean :: Buildsome -> IO ()
clean buildsome = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  Clean.Result _totalSize totalSpace count <-
    mconcat <$> mapM Clean.output (S.toList (outputs <> leaked))
  writeIORef (Db.registeredOutputsRef (bsDb buildsome)) S.empty
  writeIORef (Db.leakedOutputsRef (bsDb buildsome)) S.empty
  ColorText.putStrLn $ mconcat
    [ Color.success "Clean Successful", ": Cleaned "
    , show count, " files freeing an estimated "
    , showBytes (fromIntegral totalSpace)
    ]
