{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Buildsome.Db (Db, IRef(..), Reason)
import Buildsome.FileDescCache (getFileDesc, fileDescOfMStat)
import Buildsome.Opts (Opts(..), Opt(..))
import Control.Applicative ((<$>), Applicative(..))
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybeToList, isJust)
import Data.Monoid
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Time (DiffTime)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lib.AccessType (AccessType(..))
import Lib.AnnotatedException (annotateException)
import Lib.BuildId (BuildId)
import Lib.BuildMaps (BuildMaps(..), DirectoryBuildMap(..), TargetRep)
import Lib.ByteString (chopTrailingNewline)
import Lib.ColorText (ColorText)
import Lib.Directory (getMFileStatus, removeFileOrDirectory, removeFileOrDirectoryOrNothing, createDirectories)
import Lib.Exception (finally)
import Lib.FSHook (FSHook, Handlers(..))
import Lib.FileDesc (fileModeDescOfMStat, getFileModeDesc)
import Lib.FilePath (FilePath, (</>), (<.>))
import Lib.Fresh (Fresh)
import Lib.IORef (atomicModifyIORef'_, atomicModifyIORef_)
import Lib.Makefile (Makefile(..), TargetType(..), Target)
import Lib.Parallelism (Parallelism)
import Lib.Parsec (showPos)
import Lib.Printer (Printer, printStrLn)
import Lib.ShowBytes (showBytes)
import Lib.Sigint (installSigintHandler)
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
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Lib.BuildId as BuildId
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.Chart as Chart
import qualified Lib.ColorText as ColorText
import qualified Lib.FSHook as FSHook
import qualified Lib.FilePath as FilePath
import qualified Lib.Fresh as Fresh
import qualified Lib.Makefile as Makefile
import qualified Lib.Parallelism as Parallelism
import qualified Lib.Printer as Printer
import qualified Lib.Process as Process
import qualified Lib.Slave as Slave
import qualified Lib.StdOutputs as StdOutputs
import qualified Lib.Timeout as Timeout
import qualified Lib.Version as Version
import qualified Prelude
import qualified System.IO as IO
import qualified System.Posix.ByteString as Posix

show :: (Show a, IsString str) => a -> str
show = fromString . Prelude.show

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
  }

recordInput ::
  IORef (Map FilePath (AccessType, Reason, Maybe Posix.FileStatus)) ->
  AccessType -> Reason -> FilePath -> IO ()
recordInput inputsRef accessType reason path = do
  mstat <- getMFileStatus path
  atomicModifyIORef'_ inputsRef $
    -- Keep the older mtime in the map, and we'll eventually compare
    -- the final mtime to the oldest one
    M.insertWith merge path (accessType, reason, mstat)
  where
    merge _ (oldAccessType, oldReason, oldMStat) =
     -- Keep the highest access type, and the oldest reason/mstat
     (max accessType oldAccessType, oldReason, oldMStat)

targetShow :: Show a => a -> ColorText
targetShow = Color.target . show

renderStr :: ColorText -> String
renderStr = BS8.unpack . ColorText.render

cancelAllSlaves :: Buildsome -> IO ()
cancelAllSlaves bs = go 0
  where
    timeoutVerifyCancelled slave =
      Timeout.warning (Timeout.seconds 2)
      (unwords ["Slave", Slave.str slave, "did not cancel in 2 seconds!"]) $
      Slave.cancel slave
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
        mapM_ timeoutVerifyCancelled slaves
        -- Make sure to cancel any potential new slaves that were
        -- created during cancellation
        go count

withBuildsome :: FilePath -> Makefile -> Opt -> (Buildsome -> IO a) -> IO a
withBuildsome makefilePath makefile opt@Opt{..} body =
  FSHook.with $ \fsHook ->
  Db.with (buildDbFilename makefilePath) $ \db -> do
    slaveMapByTargetRep <- newIORef M.empty
    pool <- Parallelism.new $ fromMaybe 1 optParallelism
    freshPrinterIds <- Fresh.new 1
    buildId <- BuildId.new
    rootPath <- FilePath.canonicalizePath $ FilePath.takeDirectory makefilePath
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
        , bsParallelism = pool
        , bsFreshPrinterIds = freshPrinterIds
        }
    body buildsome
      -- We must not leak running slaves as we're not allowed to
      -- access fsHook, db, etc after leaving here:
      `E.onException` putStrLn "Shutting down"
      `finally` cancelAllSlaves buildsome
      -- Must update gitIgnore after all the slaves finished updating
      -- the registered output lists:
      `finally` maybeUpdateGitIgnore buildsome
    where
      maybeUpdateGitIgnore buildsome =
        case optUpdateGitIgnore of
        Opts.UpdateGitIgnore -> updateGitIgnore buildsome makefilePath
        Opts.DontUpdateGitIgnore -> return ()

updateGitIgnore :: Buildsome -> FilePath -> IO ()
updateGitIgnore buildsome makefilePath = do
  outputs <- readIORef $ Db.registeredOutputsRef $ bsDb buildsome
  leaked <- readIORef $ Db.leakedOutputsRef $ bsDb buildsome
  let dir = FilePath.takeDirectory makefilePath
      gitIgnorePath = dir </> ".gitignore"
      extraIgnored = [buildDbFilename makefilePath, ".gitignore"]
  putStrLn "Updating .gitignore"
  BS8.writeFile (BS8.unpack gitIgnorePath) $ BS8.unlines $
    map (("/" <>) . FilePath.makeRelative dir) $
    extraIgnored ++ S.toList (outputs <> leaked)

data BuildTargetEnv = BuildTargetEnv
  { bteBuildsome :: Buildsome
  , btePrinter :: Printer
  , bteReason :: Reason
  , bteParents :: Parents
  }

mkSlavesForPaths :: BuildTargetEnv -> Explicitness -> [FilePath] -> IO [Slave]
mkSlavesForPaths bte explicitness = fmap concat . mapM (makeSlaves bte explicitness)

want :: Printer -> Buildsome -> Reason -> [FilePath] -> IO Slave.Stats
want printer buildsome reason paths =
  fmap mconcat . mapM (Slave.wait printer) =<<
  mkSlavesForPaths BuildTargetEnv
    { bteBuildsome = buildsome
    , btePrinter = printer
    , bteReason = reason
    , bteParents = []
    } Explicit paths

assertExists :: E.Exception e => FilePath -> e -> IO ()
assertExists path err = do
  doesExist <- Posix.fileExist path
  unless doesExist $ E.throwIO err

fromBytestring8 :: IsString str => ByteString -> str
fromBytestring8 = fromString . BS8.unpack

data MissingRule = MissingRule FilePath Reason deriving (Typeable)
instance E.Exception MissingRule
instance Show MissingRule where
  show (MissingRule path reason) =
    renderStr $ Color.error $ mconcat
    ["ERROR: No rule to build ", targetShow path, " (", fromBytestring8 reason, ")"]

data TargetNotCreated = TargetNotCreated FilePath deriving (Typeable)
instance E.Exception TargetNotCreated
instance Show TargetNotCreated where
  show (TargetNotCreated path) =
    renderStr $ Color.error $ mconcat
    [ targetShow path
    , " explicitly demanded but was not created by its target rule" ]

makeDirectSlave :: BuildTargetEnv -> Explicitness -> FilePath -> IO (Maybe Slave)
makeDirectSlave bte@BuildTargetEnv{..} explicitness path =
  case BuildMaps.find (bsBuildMaps bteBuildsome) path of
  Nothing -> do
    when (explicitness == Explicit) $ assertExists path $ MissingRule path bteReason
    return Nothing
  Just tgt -> do
    slave <- getSlaveForTarget bte tgt
    Just <$> case explicitness of
      Implicit -> return slave
      Explicit -> verifyFileGetsCreated slave
  where
    verifyFileGetsCreated slave
      | path `elem` makefilePhonies (bsMakefile bteBuildsome) = return slave
      | otherwise =
        Slave.wrap (<* assertExists path (TargetNotCreated path)) slave

makeChildSlaves :: BuildTargetEnv -> FilePath -> IO [Slave]
makeChildSlaves bte@BuildTargetEnv{..} path
  | not (null childPatterns) =
    fail "UNSUPPORTED: Read directory on directory with patterns"
  | otherwise =
    traverse (getSlaveForTarget bte)
    childTargets
  where
    DirectoryBuildMap childTargets childPatterns =
      BuildMaps.findDirectory (bsBuildMaps bteBuildsome) path

makeSlavesForAccessType ::
  AccessType -> BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
makeSlavesForAccessType AccessTypeFull = makeSlaves
makeSlavesForAccessType AccessTypeModeOnly = (fmap . fmap . fmap . fmap) maybeToList makeDirectSlave

makeSlaves :: BuildTargetEnv -> Explicitness -> FilePath -> IO [Slave]
makeSlaves bte@BuildTargetEnv{..} explicitness path = do
  mSlave <- makeDirectSlave bte explicitness path
  childs <- makeChildSlaves bte { bteReason = bteReason <> " (Container directory)" } path
  return $ maybeToList mSlave ++ childs

printPosMessage :: SourcePos -> ColorText -> IO ()
printPosMessage pos msg =
  ColorText.putStrLn $ mconcat [fromString (showPos pos), ": ", msg]

-- e.g: .pyc files
handleLegalUnspecifiedOutputs :: Buildsome -> Target -> [FilePath] -> IO ()
handleLegalUnspecifiedOutputs buildsome target paths = do
  -- TODO: Verify nobody ever used this file as an input besides the
  -- creating job
  unless (null paths) $ printPosMessage (targetPos target) $
    Color.warning $ mconcat
    [ "WARNING: Leaked unspecified outputs: ", show paths, ", ", actionDesc ]
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
    warnOverSpecified "inputs" (S.fromList (targetAllInputs target))
    (inputs `S.union` phoniesSet buildsome) (targetPos target)

verifyTargetOutputs :: Buildsome -> Set FilePath -> Target -> IO ()
verifyTargetOutputs buildsome outputs target = do
  -- Legal unspecified need to be kept/deleted according to policy:
  handleLegalUnspecifiedOutputs buildsome target =<<
    filterM Posix.fileExist unspecifiedOutputs

  -- Illegal unspecified that no longer exist need to be banned from
  -- input use by any other job:
  -- TODO: Add to a ban-from-input-list (by other jobs)

  -- Illegal unspecified that do exist are a problem:
  existingIllegalOutputs <- filterM Posix.fileExist illegalOutputs
  unless (null existingIllegalOutputs) $ do
    printPosMessage (targetPos target) $ Color.error $
      "Illegal output files created: " <> show existingIllegalOutputs
    mapM_ removeFileOrDirectory existingIllegalOutputs
    E.throwIO $ IllegalUnspecifiedOutputs target existingIllegalOutputs
  warnOverSpecified "outputs" specified (outputs `S.union` phoniesSet buildsome) (targetPos target)
  where
    (unspecifiedOutputs, illegalOutputs) =
      partition MagicFiles.allowedUnspecifiedOutput allUnspecified
    allUnspecified = S.toList $ outputs `S.difference` specified
    specified = S.fromList $ targetOutputs target

warnOverSpecified ::
  ColorText -> Set FilePath -> Set FilePath -> SourcePos -> IO ()
warnOverSpecified str specified used pos =
  unless (S.null unused) $
  printPosMessage pos $ Color.warning $ mconcat
  [ "WARNING: Over-specified ", str, ": ", targetShow (S.toList unused) ]
  where
    unused = specified `S.difference` used

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

targetPrintWrap :: BuildTargetEnv -> Target -> ColorText -> IO a -> IO a
targetPrintWrap BuildTargetEnv{..} target str body =
  Printer.printWrap btePrinter (targetShow (targetOutputs target)) $ do
    printStrLn btePrinter $ mconcat [str, " (", fromBytestring8 bteReason, ")"]
    body

showMultiline :: ByteString -> ByteString
showMultiline xs
  | '\n' `BS8.notElem` x = x
  | otherwise = multilineDelimiter <> x <> multilineDelimiter
  where
    x = chopTrailingNewline xs
    multilineDelimiter = "\"\"\""

printCmd :: Printer -> Target -> IO ()
printCmd printer target =
  unless (BS8.null cmd) $ printStrLn printer $ showMultiline $
  ColorText.render $ Color.command $ fromBytestring8 cmd
  where
    cmd = targetCmds target

colorStdOutputs :: StdOutputs ByteString -> StdOutputs ColorText
colorStdOutputs (StdOutputs out err) =
  StdOutputs
  (colorize Color.stdout out)
  (colorize Color.stderr err)
  where
    colorize f = f . fromBytestring8 . chopTrailingNewline

outputsStr :: ColorText -> StdOutputs ByteString -> Maybe ColorText
outputsStr label = StdOutputs.str label . colorStdOutputs

printTargetStdOutputs :: Target -> StdOutputs ByteString -> IO ()
printTargetStdOutputs target stdOutputs =
  maybe (return ()) ColorText.putStrLn $
  outputsStr (targetShow (targetOutputs target)) stdOutputs

-- Already verified that the execution log is a match
applyExecutionLog ::
  BuildTargetEnv -> Target ->
  Set FilePath -> Set FilePath ->
  StdOutputs ByteString -> DiffTime -> IO ()
applyExecutionLog bte@BuildTargetEnv{..} target inputs outputs stdOutputs selfTime =
  targetPrintWrap bte target "REPLAY" $ do
    printCmd btePrinter target
    printTargetStdOutputs target stdOutputs
    verifyTargetSpec bteBuildsome inputs outputs target
    printStrLn btePrinter $ ColorText.render $
      "Build (originally) took " <> Color.timing (show selfTime <> " seconds")

waitForSlaves :: Printer -> Parallelism.Cell -> Buildsome -> [Slave] -> IO Slave.Stats
waitForSlaves _ _ _ [] = return mempty
waitForSlaves printer parCell buildsome slaves =
  Parallelism.withReleased parCell (bsParallelism buildsome) $
  mconcat <$> mapM (Slave.wait printer) slaves

tryApplyExecutionLog ::
  BuildTargetEnv -> Parallelism.Cell ->
  TargetRep -> Target -> Db.ExecutionLog ->
  IO (Either (ByteString, FilePath) Slave.Stats)
tryApplyExecutionLog bte@BuildTargetEnv{..} parCell targetRep target Db.ExecutionLog {..} = do
  nestedSlaveStats <- waitForInputs
  runEitherT $ do
    forM_ (M.toList elInputsDescs) $ \(filePath, oldInputAccess) ->
      case snd oldInputAccess of
        Db.InputAccessFull     oldDesc     -> compareToNewDesc "input"       (getFileDesc db) (filePath, oldDesc)
        Db.InputAccessModeOnly oldModeDesc -> compareToNewDesc "input(mode)" getFileModeDesc  (filePath, oldModeDesc)
        Db.InputAccessIgnoredFile          -> return ()
    -- For now, we don't store the output files' content
    -- anywhere besides the actual output files, so just verify
    -- the output content is still correct
    mapM_ (compareToNewDesc "output" (getFileDesc db)) $ M.toList elOutputsDescs

    liftIO $
      applyExecutionLog bte target
      (M.keysSet elInputsDescs) (M.keysSet elOutputsDescs)
      elStdoutputs elSelfTime
    let selfStats = Slave.Stats $ M.singleton targetRep (Slave.FromCache, elSelfTime)
    return $ mappend selfStats nestedSlaveStats
  where
    db = bsDb bteBuildsome
    compareToNewDesc str getNewDesc (filePath, oldDesc) = do
      newDesc <- liftIO $ getNewDesc filePath
      when (oldDesc /= newDesc) $ left (str, filePath) -- fail entire computation
    inputAccessToType Db.InputAccessModeOnly {} = Just AccessTypeModeOnly
    inputAccessToType Db.InputAccessFull {} = Just AccessTypeFull
    inputAccessToType Db.InputAccessIgnoredFile = Nothing
    waitForInputs = do
      -- TODO: This is good for parallelism, but bad if the set of
      -- inputs changed, as it may build stuff that's no longer
      -- required:
      let hinted = S.fromList $ targetAllInputs target
      speculativeSlaves <-
        fmap concat $ forM (M.toList elInputsDescs) $ \(inputPath, (depReason, inputAccess)) ->
        if inputPath `S.member` hinted
        then return []
        else case inputAccessToType inputAccess of
          Nothing -> return []
          Just accessType ->
            makeSlavesForAccessType accessType
            bte { bteReason = depReason } Implicit inputPath

      let hintReason = ColorText.render $ "Hint from " <> (targetShow . targetOutputs) target
      hintedSlaves <-
        mkSlavesForPaths bte { bteReason = hintReason } Explicit $ targetAllInputs target

      let allSlaves = speculativeSlaves ++ hintedSlaves
      waitForSlaves btePrinter parCell bteBuildsome allSlaves

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
            , Color.path (show filePath), " changed"
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
        , mkSlave newSlaveMVar $ \printer getParId ->
          E.bracket (Parallelism.newCell =<< getParId) (Parallelism.release par) $
          \parCell -> restoreMask $ do
            let newBte = bte { bteParents = newParents, btePrinter = printer }
            mTime <- findApplyExecutionLog newBte parCell targetRep target
            case mTime of
              Nothing -> buildTarget newBte parCell targetRep target
              Just time -> return time
        )
    where
      par = bsParallelism bteBuildsome
      annotate =
        annotateException $ renderStr $
        "build failure of " <> targetShow (targetOutputs target) <> ":\n"
      newParents = (targetRep, bteReason) : bteParents
      panicHandler e@E.SomeException {} = panic $ "FAILED during making of slave: " ++ show e
      mkSlave mvar action = do
        slave <-
          E.handle panicHandler $ do
            getParId <- Parallelism.startAlloc $ bsParallelism bteBuildsome
            depPrinterId <- Fresh.next $ bsFreshPrinterIds bteBuildsome
            depPrinter <- Printer.newFrom btePrinter depPrinterId
            Slave.new depPrinterId (targetOutputs target) $ annotate $ action depPrinter getParId
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
  , rcrInputs :: Map FilePath (AccessType, Reason, Maybe Posix.FileStatus)
  , rcrOutputs :: Set FilePath
  , rcrSlaveStats :: Slave.Stats
  }

runCmd :: BuildTargetEnv -> Parallelism.Cell -> Target -> IO RunCmdResults
runCmd bte@BuildTargetEnv{..} parCell target = do
  pauseTime <- newIORef 0
  inputsRef <- newIORef M.empty
  outputsRef <- newIORef S.empty
  slaveStatsRef <- newIORef mempty
  let
    addPauseTime delta = atomicModifyIORef'_ pauseTime (+delta)
    mappendSlaveStats stats = atomicModifyIORef_ slaveStatsRef (mappend stats)
    measurePauseTime act = do
      (time, res) <- timeIt act
      addPauseTime time
      return res
    handleInputCommon accessType actDesc path useInput
      | MagicFiles.inputIgnored path = return ()
      | otherwise = do
        actualOutputs <- readIORef outputsRef
        -- There's no problem for a target to read its own outputs freely:
        unless (any (path `S.member`) [actualOutputs, targetOutputsSet]) $ do
          () <- useInput
          recordInput inputsRef accessType actDesc path
    handleInput accessType actDesc path =
      handleInputCommon accessType actDesc path $ return ()
    handleDelayedInput accessType actDesc path =
      handleInputCommon accessType actDesc path $
        measurePauseTime . (>>= mappendSlaveStats) . waitForSlaves btePrinter parCell bteBuildsome =<<
        makeSlavesForAccessType accessType bte { bteReason = actDesc } Implicit path
    handleOutput _actDesc path
      | MagicFiles.outputIgnored path = return ()
      | otherwise = atomicModifyIORef'_ outputsRef $ S.insert path
  printCmd btePrinter target
  (time, stdOutputs) <-
    FSHook.runCommand (bsFsHook bteBuildsome) (bsRootPath bteBuildsome)
    (timeIt . shellCmdVerify target ["HOME", "PATH"])
    (ColorText.render (targetShow (targetOutputs target)))
    Handlers {..}
    `finally` do
      outputs <- readIORef outputsRef
      registerOutputs bteBuildsome $ S.intersection outputs targetOutputsSet
  subtractedTime <- (time-) <$> readIORef pauseTime
  inputs <- readIORef inputsRef
  outputs <- readIORef outputsRef
  slaveStats <- readIORef slaveStatsRef
  return RunCmdResults
    { rcrStdOutputs = stdOutputs
    , rcrSelfTime = realToFrac subtractedTime
    , rcrInputs = inputs
    , rcrOutputs = outputs
    , rcrSlaveStats = slaveStats
    }
  where
    targetOutputsSet = S.fromList $ targetOutputs target

saveExecutionLog :: Buildsome -> Target -> RunCmdResults -> IO ()
saveExecutionLog buildsome target RunCmdResults{..} = do
  inputsDescs <- M.traverseWithKey inputAccess rcrInputs
  outputDescPairs <-
    forM (S.toList rcrOutputs) $ \outPath -> do
      fileDesc <- getFileDesc db outPath
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
    inputAccess path (_, reason, _) | MagicFiles.allowedUnspecifiedOutput path = return (reason, Db.InputAccessIgnoredFile)
    inputAccess path (AccessTypeFull, reason, mStat) = (,) reason . Db.InputAccessFull <$> fileDescOfMStat db path mStat
    inputAccess path (AccessTypeModeOnly, reason, mStat) = (,) reason . Db.InputAccessModeOnly <$> fileModeDescOfMStat path mStat

buildTarget :: BuildTargetEnv -> Parallelism.Cell -> TargetRep -> Target -> IO Slave.Stats
buildTarget bte@BuildTargetEnv{..} parCell targetRep target =
  targetPrintWrap bte target "BUILDING" $ do
    -- TODO: Register each created subdirectory as an output?
    mapM_ (createDirectories . FilePath.takeDirectory) $ targetOutputs target

    registeredOutputs <- readIORef $ Db.registeredOutputsRef $ bsDb bteBuildsome
    mapM_ (removeOldOutput btePrinter bteBuildsome registeredOutputs) $ targetOutputs target

    slaves <-
      mkSlavesForPaths bte
        { bteReason = ColorText.render $ "Hint from " <> targetShow (targetOutputs target)
        } Explicit $ targetAllInputs target
    hintedStats <- waitForSlaves btePrinter parCell bteBuildsome slaves

    rcr@RunCmdResults{..} <- runCmd bte parCell target

    verifyTargetSpec bteBuildsome (M.keysSet rcrInputs) rcrOutputs target
    saveExecutionLog bteBuildsome target rcr

    printStrLn btePrinter $
      "Build (now) took " <> Color.timing (show rcrSelfTime <> " seconds")
    let selfStats = Slave.Stats $ M.singleton targetRep (Slave.BuiltNow, rcrSelfTime)
    return $ mconcat [selfStats, hintedStats, rcrSlaveStats]

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
    [ fromBytestring8 (showMultiline cmd), " failed: ", show exitCode
    , fromMaybe "" $ outputsStr "Outputs:" stdOutputs ]
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
  printTargetStdOutputs target stdOutputs
  return stdOutputs

buildDbFilename :: FilePath -> FilePath
buildDbFilename = (<.> "db")

standardMakeFilename :: FilePath
standardMakeFilename = "Makefile"

data MakefileScanFailed = MakefileScanFailed deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show MakefileScanFailed =
    renderStr $ Color.error $ mconcat
    [ "ERROR: Cannot find a file named "
    , Color.path (show standardMakeFilename)
    , " in this directory or any of its parents"
    ]

findMakefile :: IO FilePath
findMakefile = do
  cwd <- Posix.getWorkingDirectory
  let
    -- NOTE: Excludes root (which is probably fine)
    parents = takeWhile (/= "/") $ iterate FilePath.takeDirectory cwd
    candidates = map (</> standardMakeFilename) parents
  -- Use EitherT with Left short-circuiting when found, and Right
  -- falling through to the end of the loop:
  res <- runEitherT $ mapM_ check candidates
  case res of
    Left found -> FilePath.makeRelativeToCurrentDirectory found
    Right () -> E.throwIO MakefileScanFailed
  where
    check path = do
      exists <- liftIO $ Posix.fileExist path
      when exists $ left path

data SpecifiedInexistentMakefilePath = SpecifiedInexistentMakefilePath FilePath deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath path) =
    renderStr $ Color.error $ mconcat
    ["Specified makefile path: ", Color.path (show path), " does not exist"]
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: FilePath -> IO FilePath
specifiedMakefile path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath path
    Just stat
      | Posix.isDirectory stat -> return $ path </> "Makefile"
      | otherwise -> return path

data TargetsRequest = TargetsRequest
  { targetsRequestPaths :: [FilePath]
  , targetsRequestReason :: Reason
  , targetsRequestChartPath :: Maybe FilePath
  }

data Requested = RequestedClean | RequestedTargets TargetsRequest

data BadCommandLine = BadCommandLine String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine msg) =
    renderStr $ Color.error $ "Invalid command line options: " <> fromString msg

getRequestedTargets :: Maybe FilePath -> [ByteString] -> IO Requested
getRequestedTargets (Just _) ["clean"] = E.throwIO $ BadCommandLine "Clean requested with charts"
getRequestedTargets Nothing ["clean"] = return RequestedClean
getRequestedTargets mChartPath [] = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ["default"]
  , targetsRequestReason = "implicit 'default' target"
  , targetsRequestChartPath = mChartPath }
getRequestedTargets mChartPath ts
  | "clean" `elem` ts = E.throwIO $ BadCommandLine "Clean must be requested exclusively"
  | otherwise = return $ RequestedTargets TargetsRequest
  { targetsRequestPaths = ts
  , targetsRequestReason = "explicit request from cmdline"
  , targetsRequestChartPath = mChartPath }

setBuffering :: IO ()
setBuffering = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

switchDirectory :: FilePath -> IO (FilePath, FilePath)
switchDirectory makefilePath = do
  origCwd <- Posix.getWorkingDirectory
  unless (BS8.null cwd) $ do
    Posix.changeWorkingDirectory cwd
    fullCwd <- FilePath.canonicalizePath $ origCwd </> cwd
    BS8.putStrLn $ "make: Entering directory `" <> fullCwd <> "'"
  return (origCwd, file)
  where
    (cwd, file) = FilePath.splitFileName makefilePath

parseMakefile :: FilePath -> FilePath -> IO Makefile
parseMakefile origMakefilePath finalMakefilePath = do
  (parseTime, makefile) <- timeIt $ Makefile.onMakefilePaths FilePath.canonicalizePathAsRelative =<< Makefile.parse finalMakefilePath
  ColorText.putStrLn $ mconcat
    [ "Parsed makefile: ", Color.path (show origMakefilePath)
    , " (took ", Color.timing (show parseTime <> "sec"), ")"]
  return makefile

type InOrigCwd = [FilePath] -> IO [FilePath]

handleOpts :: Opts -> IO (Maybe (Opt, InOrigCwd, Requested, FilePath, Makefile))
handleOpts GetVersion = do
  ver <- Version.get
  BS8.putStrLn $ "buildsome " <> ver
  return Nothing
handleOpts (Opts opt) = do
  origMakefilePath <- maybe findMakefile specifiedMakefile $ optMakefilePath opt
  mChartsPath <- traverse FilePath.canonicalizePath $ optChartsPath opt
  (origCwd, finalMakefilePath) <- switchDirectory origMakefilePath
  let inOrigCwd =
        case optMakefilePath opt of
        -- If we found the makefile by scanning upwards, prepend
        -- original cwd to avoid losing it:
        Nothing -> mapM (FilePath.canonicalizePathAsRelative . (origCwd </>))
        -- Otherwise: there's no useful original cwd:
        Just _ -> return
  requested <- getRequestedTargets mChartsPath $ optRequestedTargets opt
  makefile <- parseMakefile origMakefilePath finalMakefilePath
  return $ Just (opt, inOrigCwd, requested, finalMakefilePath, makefile)

makeChart :: Slave.Stats -> FilePath -> IO ()
makeChart slaveStats filePath = do
  putStrLn $ "Writing chart to " ++ show filePath
  Chart.make slaveStats filePath

handleRequested :: Buildsome -> Printer -> InOrigCwd -> Requested -> IO ()
handleRequested buildsome printer inOrigCwd (RequestedTargets (TargetsRequest requestedTargets reason mChartPath)) = do
  requestedTargetPaths <- inOrigCwd requestedTargets
  printStrLn printer $
    "Building: " <> ColorText.intercalate ", " (map targetShow requestedTargetPaths)
  (buildTime, slaveStats) <- timeIt $ want printer buildsome reason requestedTargetPaths
  printStrLn printer $ mconcat
    [ Color.success "Build Successful", ": "
    , Color.timing (show buildTime <> " seconds"), " total." ]
  maybe (return ()) (makeChart slaveStats) mChartPath

handleRequested buildsome _ _ RequestedClean = do
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

main :: IO ()
main = do
  opts <- Opts.get
  mRes <- handleOpts opts
  case mRes of
    Nothing -> return ()
    Just (opt, inOrigCwd, requested, finalMakefilePath, makefile) -> do
      installSigintHandler
      setBuffering
      printer <- Printer.new 0
      withBuildsome finalMakefilePath makefile opt $ \buildsome -> do
        deleteRemovedOutputs buildsome
        handleRequested buildsome printer inOrigCwd requested
