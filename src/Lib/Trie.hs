{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Trie
       ( Trie(..)
       , prettyTrieSummary
       , lookup
       )
       where

import           Lib.NonEmptyList (NonEmptyList(..))
import           Lib.NonEmptyMap (NonEmptyMap)
import qualified Lib.NonEmptyMap as NonEmptyMap

import           Data.Foldable (asum)
import           Control.Monad.Trans.Either (EitherT(..), runEitherT)
import           Data.Binary (Binary)
import           GHC.Generics (Generic)

import           Prelude.Compat hiding (lookup)

-- | A non-empty trie with values only at leaves, and multiple payloads ("descs") for each key
data Trie key keyDesc value tree
  = Branch (NonEmptyMap key (NonEmptyMap keyDesc tree))
  | Leaf value
  deriving (Generic, Show, Functor, Foldable, Traversable)
instance (Binary key, Binary keyDesc, Binary tree, Binary value)
  => Binary (Trie key keyDesc value tree)

prettyTrieSummary ::
  (Show key) =>
  (tree -> Trie key keyDesc value tree) ->
  tree ->
  String
prettyTrieSummary unfix = go ""
  where
    go indent node =
      case unfix node of
      Leaf{} -> "Leaf"
      Branch m ->
        mconcat
        [ "\n", indent, ('>' :) . concatMap (uncurry showLeaf) $ NonEmptyMap.toList m ]
        where
          showLeaf leaf branches =
            case NonEmptyMap.toNonEmptyList branches of
            NonEmptyList x [] -> mconcat [show leaf, go t $ snd x]
            NonEmptyList x xs ->
                -- TODO: List comprehension with unlines
                concatMap
                ((mconcat ["\n", t, show leaf, ":"] ++) . go t . snd)
                (x:xs)
          t = ' ' : indent

firstRightAction ::
  (Applicative m, Monad m, Functor t, Foldable t, Monoid e) =>
  t (m (Either e a)) -> m (Either e a)
firstRightAction = runEitherT . asum . fmap EitherT

bimapEither :: (e -> e') -> (a -> a') -> Either e a -> Either e' a'
bimapEither f _ (Left x)  = Left $ f x
bimapEither _ g (Right x) = Right $ g x

annotateMatches ::
  a -> b -> Either [reason] () ->
  Either [(b, reason)] (a, b)
annotateMatches keyDesc value =
  bimapEither (map (value,)) (const (keyDesc, value))

checkBranches :: (Functor f) =>
  (key -> keyResult -> keyDesc -> f (Either [reason] ())) ->
  key -> keyResult ->
  NonEmptyMap.NonEmptyMap keyDesc b ->
  NonEmptyList (f (Either [(b, reason)] (keyDesc, b)))
checkBranches checkMatch filePath mStat branches =
  mapMatch <$> NonEmptyMap.toNonEmptyList branches
  where
    mapMatch (keyDesc, value) =
      annotateMatches keyDesc value <$> checkMatch filePath mStat keyDesc

lookupLeaf ::
  (Functor m, Applicative m, Monad m) =>
  (tree -> Trie key keyDesc value tree)
  -> (key -> m keyResult)
  -> (key -> keyResult -> keyDesc -> m (Either [reason] ()))
  -> (key, NonEmptyMap keyDesc tree)
  -> m (Either [(key, reason)] value)
lookupLeaf unfix branchAct checkMatch (key, branches) = do
  keyResult <- branchAct key
  match <- firstRightAction $ checkBranches checkMatch key keyResult branches
  case match of
    -- include the input path in the error, for caller's convenience
    Left results ->
        Left
        . (map ((key, ) . snd) results ++)
        . mconcat . map (either id (const []))
        <$> traverse (lookup' . fst) results

    Right (_, elt) -> lookup' elt
  where
    lookup' = lookup unfix branchAct checkMatch

lookup ::
  (Applicative m, Monad m) =>
  (tree -> Trie key keyDesc value tree)
  -> (key -> m keyResult)
  -> (key -> keyResult -> keyDesc -> m (Either [reason] ()))
  -> tree
  -> m (Either [(key, reason)] value)
lookup unfix branchAct checkMatch node =
  case unfix node of
  Leaf el -> return $ Right el
  Branch branches ->
    firstRightAction
    . map (lookupLeaf unfix branchAct checkMatch)
    $ NonEmptyMap.toList branches
