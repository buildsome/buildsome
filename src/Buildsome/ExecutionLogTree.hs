{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Buildsome.ExecutionLogTree
  ( lookup
  , append
  , fromExecutionLog
  , showTreeSummary
  )
  where

import           Buildsome.Db            (ExecutionLog (..),
                                          ExecutionLogTree (..),
                                          ExecutionLogTreeInput (..),
                                          FileDesc (..), InputDesc (..),
                                          InputLog (..), InputLogStat (..),
                                          bimapFileDesc)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Data.Foldable           (asum)
import           Data.List               (intercalate)
import qualified Data.Map                as Map
import qualified Lib.Directory           as Dir
import           Lib.FileDesc            (FileStatDesc (..),
                                          FullStatEssence (..),
                                          FileModeDesc (..),
                                          FileContentDesc (..),
                                          fileContentDescOfStat,
                                          fileModeDescOfStat,
                                          fileStatDescOfStat,
                                          fileStatDescOfStat)
import           Lib.FilePath (FilePath)
import qualified Lib.NonEmptyMap as NonEmptyMap
import qualified System.Posix.ByteString as Posix
import           Prelude.Compat hiding (FilePath, lookup)

fileDescToInputLogStat :: FileStatDesc -> InputLogStat
fileDescToInputLogStat (FileStatDirectory stat) =
  InputLogStat
  { ilsBasicStatEssence = stat
  , ilsFileSize = Nothing --fileSize statDesc
  , ilsFileType = Nothing -- fileType statDesc
  }
fileDescToInputLogStat (FileStatOther stat) =
  InputLogStat
  { ilsBasicStatEssence = basicStatEssence stat
  , ilsFileSize = Just $ fileSize stat
  , ilsFileType = Just $ fileType stat
  }

inputDescToInputLog :: InputDesc -> InputLog
inputDescToInputLog InputDesc{..} =
  InputLog
  { ilModeAccess = snd <$> idModeAccess
  , ilStatAccess = fileDescToInputLogStat . snd <$> idStatAccess
  , ilContentAccess = snd <$> idContentAccess
  }

data MismatchReason
  = MismatchExpectedExisting
  | MismatchExpectedNonExisting
  | MismatchStatDiffers InputLogStat InputLogStat
  | MismatchModeDiffers FileModeDesc FileModeDesc
  | MismatchContentDiffers FileContentDesc FileContentDesc
  deriving (Show, Eq, Ord)

andRight :: [Either a t] -> Either a ()
andRight [] = Right ()
andRight (Left x : _) = Left x
andRight (Right _ : xs) = andRight xs

matchesCurrentFS
  :: (Functor m, Applicative m, MonadIO m)
  => FilePath -> Maybe Posix.FileStatus -> FileDesc t InputLog
  -> m (Either [MismatchReason] ())
matchesCurrentFS filePath mStat inputDesc =
  case (inputDesc, mStat) of
  (FileDescNonExisting{}, Nothing) -> return $ Right ()
  (FileDescNonExisting{}, Just _ ) -> return $ Left [MismatchExpectedNonExisting]
  (FileDescExisting{}   , Nothing) -> return $ Left [MismatchExpectedExisting]
  (FileDescExisting inputFileDesc, Just stat) ->
    andRight <$> sequenceA
    [ compareProp (ilModeAccess inputFileDesc)    (return $ fileModeDescOfStat stat) MismatchModeDiffers
    , compareProp (ilStatAccess inputFileDesc)    (return . fileDescToInputLogStat $ fileStatDescOfStat stat) MismatchStatDiffers
    , compareProp (ilContentAccess inputFileDesc) (liftIO $ fileContentDescOfStat filePath stat) MismatchContentDiffers
    ]
  where compareProp prop act err = maybe (return $ Right ()) (check act err) prop
        check act err x = do
          res <- act
          if x == res then return (Right ()) else return (Left [err x res])

firstRightAction
  :: (Applicative m, Monad m, Functor t, Foldable t, Monoid e)
  => t (m (Either e a)) -> m (Either e a)
firstRightAction = runEitherT . asum . fmap EitherT

mapRight :: (r1 -> r2) -> Either a r1 -> Either a r2
mapRight _ (Left e) = Left e
mapRight f (Right r) = Right $ f r

lookupInput :: FilePath -> ExecutionLogTreeInput -> IO (Either [(FilePath, MismatchReason)] ExecutionLog)
lookupInput filePath ExecutionLogTreeInput{..} = do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  let matches =
        map (\x -> fmap (mapRight (const x)) <$> matchesCurrentFS filePath mStat $ fst x)
        $ NonEmptyMap.toList eltiBranches
  match <- firstRightAction matches
  case match of
    -- include the input path in the error, for caller's convenience
    Left reasons -> return . Left $ map (filePath,) reasons
    Right (_, elt) -> lookup elt

lookup :: ExecutionLogTree -> IO (Either [(FilePath, MismatchReason)] ExecutionLog)
lookup (ExecutionLogLeaf el) = return $ Right el
lookup (ExecutionLogBranch inputs) =
  firstRightAction
  . map (uncurry lookupInput)
  . NonEmptyMap.toList
  $ inputs

getInputLogs :: ExecutionLog -> [(FilePath, FileDesc () InputLog)]
getInputLogs ExecutionLog{..}
  = Map.toList
  -- 1. Throw away the Reason by mapping it to ()
  -- 2. Convert (mtime, InputDesc) to InputLog
  . fmap (bimapFileDesc (const ()) (inputDescToInputLog . snd))
  $ elInputsDescs

fromExecutionLog :: ExecutionLog -> ExecutionLogTree
fromExecutionLog el = fromExecutionLog' el $ getInputLogs el

fromExecutionLog' :: ExecutionLog -> [(FilePath, FileDesc () InputLog)] -> ExecutionLogTree
fromExecutionLog' el [] = ExecutionLogLeaf el
fromExecutionLog' el ((filePath, inputDesc) : inputs) =
  ExecutionLogBranch
  . NonEmptyMap.singleton filePath
  . ExecutionLogTreeInput
  $ NonEmptyMap.singleton inputDesc (fromExecutionLog' el inputs)

append :: ExecutionLog -> ExecutionLogTree -> ExecutionLogTree
append el elt' = go (getInputLogs el, el) elt' []
  where
    go ([], new) ExecutionLogLeaf{} _ = ExecutionLogLeaf new
    go ((filePath,_):_, _) ExecutionLogLeaf{} oldInputs
      = error $ intercalate "\n\t"
        [ "Existing execution log with no further inputs exists, but new one has more inputs!"
        , "Target: ", show $ elOutputsDescs el
        , "Target command: ", show $ elCommand el
        , "Example extra input: ", show filePath
        , "Existing entry's inputs: ", concatMap (("\n\t\t" ++) . show) oldInputs
        ]
    go ([], _) ExecutionLogBranch{} _
      = error "Existing execution log has more inputs, but new one has no further inputs!"
    go ((filePath, inputDesc) : is, new) (ExecutionLogBranch inputses) oldInputs
      = case NonEmptyMap.lookup filePath inputses of
          -- no filepath matching this input at current level
          Nothing -> ExecutionLogBranch $ NonEmptyMap.insert filePath newInput inputses
            where newInput = ExecutionLogTreeInput (NonEmptyMap.singleton inputDesc $ fromExecutionLog' new is)

          -- found an input with the same file path, need to check
          -- it's filedesc
          Just ExecutionLogTreeInput{..} ->
            case NonEmptyMap.lookup inputDesc eltiBranches of
              -- none of the existing filedescs matches what we have
              Nothing -> ExecutionLogBranch $ NonEmptyMap.insert filePath newInput inputses
                where
                  newInput = ExecutionLogTreeInput
                           $ NonEmptyMap.insert inputDesc (fromExecutionLog' new is) eltiBranches

              -- found exact match, go down the tree
              Just next -> go (is, new) next (filePath:oldInputs)

showTreeSummary :: ExecutionLogTree -> [Char]
showTreeSummary = showTreeSummary' ""

showTreeSummary' :: [Char] -> ExecutionLogTree -> [Char]
showTreeSummary' _   ExecutionLogLeaf{} = "Leaf"
showTreeSummary' tab (ExecutionLogBranch m) =
  mconcat
  [ "\n", tab, ('>' :) . concatMap (uncurry showInput) $ NonEmptyMap.toList m ]
  where showInput input ExecutionLogTreeInput{..} =
          case branches of
            []  -> error "can't be empty"
            [x] -> mconcat [show input, showTreeSummary' (t : tab) $ snd x]
            xs  -> concatMap ((mconcat ["\n", t : tab, show input, ":"] ++) . showTreeSummary' (t : tab) . snd) $ xs
          where branches = NonEmptyMap.toList eltiBranches
        t = ' '

