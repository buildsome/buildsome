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
                                          FileDescInput)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Data.Foldable (asum)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Lib.Cmp (Cmp(..), ComparisonResult(..), Reasons)
import qualified Lib.Directory as Dir
import           Lib.FileDesc            (fileContentDescOfStat,
                                          fileModeDescOfStat,
                                          fileStatDescOfStat,
                                          fileStatDescOfStat)
import           Lib.FilePath (FilePath)
import           Lib.NonEmptyList (NonEmptyList(..))
import           Lib.NonEmptyMap (NonEmptyMap)
import qualified Lib.NonEmptyMap as NonEmptyMap
import           Prelude.Compat hiding (FilePath, lookup)
import qualified System.Posix.ByteString as Posix

-- fileDescToInputLogStat :: FileStatDesc -> InputLogStat
-- fileDescToInputLogStat (FileStatDirectory stat) =
--   InputLogStat
--   { ilsBasicStatEssence = stat
--   , ilsFileSize = Nothing --fileSize statDesc
--   , ilsFileType = Nothing -- fileType statDesc
--   }
-- fileDescToInputLogStat (FileStatOther stat) =
--   InputLogStat
--   { ilsBasicStatEssence = basicStatEssence stat
--   , ilsFileSize = Just $ fileSize stat
--   , ilsFileType = Just $ fileType stat
--   }

-- inputDescToInputLog :: InputDesc -> InputLog
-- inputDescToInputLog InputDesc{..} =
--   InputLog
--   { ilModeAccess = snd <$> idModeAccess
--   , ilStatAccess = fileDescToInputLogStat . snd <$> idStatAccess
--   , ilContentAccess = snd <$> idContentAccess
--   }

data MismatchReason
  = MismatchExpectedExisting
  | MismatchExpectedNonExisting
  | MismatchFileDiffers Reasons
  deriving (Show)

andRight :: [Either a t] -> Either a ()
andRight [] = Right ()
andRight (Left x : _) = Left x
andRight (Right _ : xs) = andRight xs

matchesCurrentFS
  :: (Functor m, Applicative m, MonadIO m)
  => FilePath -> Maybe Posix.FileStatus -> FileDescInput
  -> m (Either [MismatchReason] ())
matchesCurrentFS filePath mStat inputDesc =
  case (inputDesc, mStat) of
  (FileDescNonExisting{}, Nothing) -> return $ Right ()
  (FileDescNonExisting{}, Just _ ) -> return $ Left [MismatchExpectedNonExisting]
  (FileDescExisting{}   , Nothing) -> return $ Left [MismatchExpectedExisting]
  (FileDescExisting (_mtime, inputFileDesc), Just stat) ->
    andRight <$> sequenceA
    [ compareProp (idModeAccess inputFileDesc)    (return $ fileModeDescOfStat stat)
    , compareProp (idStatAccess inputFileDesc)    (return $ fileStatDescOfStat stat)
    , compareProp (idContentAccess inputFileDesc) (liftIO $ fileContentDescOfStat filePath stat)
    ]

compareProp
 :: (Monad m, Cmp a)
 => Maybe (a1, a) -> m a -> m (Either [MismatchReason] ())
compareProp prop act =
  maybe (return $ Right ()) check $ fmap snd prop
    where
      check x = do
        res <- act
        case x `cmp` res of
          Equals -> return (Right ())
          NotEquals r -> return (Left [MismatchFileDiffers r])

firstRightAction
  :: (Applicative m, Monad m, Functor t, Foldable t, Monoid e)
  => t (m (Either e a)) -> m (Either e a)
firstRightAction = runEitherT . asum . fmap EitherT

bimapEither :: (e -> e') -> (a -> a') -> Either e a -> Either e' a'
bimapEither f _ (Left x)  = Left $ f x
bimapEither _ g (Right x) = Right $ g x

checkBranches ::
    MonadIO f =>
    FilePath -> Maybe Posix.FileStatus -> NonEmptyMap FileDescInput b ->
    NonEmptyList (f (Either [(b, MismatchReason)] (FileDescInput, b)))
checkBranches filePath mStat branches =
  fmap (\x -> fmap (bimapEither (map (snd x,)) (const x)) <$> matchesCurrentFS filePath mStat $ fst x)
  $ NonEmptyMap.toNonEmptyList branches

lookupInput :: FilePath -> ExecutionLogTreeInput -> IO (Either [(FilePath, MismatchReason)] ExecutionLog)
lookupInput filePath ExecutionLogTreeInput{..} = do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  match <- firstRightAction $ checkBranches filePath mStat eltiBranches
  case match of
    -- include the input path in the error, for caller's convenience
    Left results -> do
      branchResults <- mapM (lookup . fst) results
      return
        $ Left
        . ((map ((filePath, ) . snd) results) ++)
        $ mconcat $ map (either id (const [])) branchResults

    Right (_, elt) -> lookup elt

lookup :: ExecutionLogTree -> IO (Either [(FilePath, MismatchReason)] ExecutionLog)
lookup (ExecutionLogLeaf el)       = return $ Right el
lookup (ExecutionLogBranch inputs) =
  firstRightAction
  . map (uncurry lookupInput)
  . NonEmptyMap.toList
  $ inputs

inputsList :: ExecutionLog -> [(FilePath, FileDescInput)]
inputsList ExecutionLog{..} = Map.toList elInputsDescs

fromExecutionLog :: ExecutionLog -> ExecutionLogTree
fromExecutionLog el = fromExecutionLog' el $ inputsList el

fromExecutionLog' :: ExecutionLog -> [(FilePath, FileDescInput)] -> ExecutionLogTree
fromExecutionLog' el [] = ExecutionLogLeaf el
fromExecutionLog' el ((filePath, inputDesc) : inputs) =
  ExecutionLogBranch
  . NonEmptyMap.singleton filePath
  . ExecutionLogTreeInput
  $ NonEmptyMap.singleton inputDesc (fromExecutionLog' el inputs)

append :: ExecutionLog -> ExecutionLogTree -> ExecutionLogTree
append el elt' = go (inputsList el, el) elt' []
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

