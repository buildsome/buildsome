{-# LANGUAGE RecordWildCards #-}
module Lib.Makefile.CondState
  ( CondState, empty
  , isTrue
  , nest
  , unnest
  , inverse
  ) where

import Prelude.Compat

data CondState = CondState
  { trueNesting :: Int -- ignored unless false=0
  , falseNesting :: Int
  }

empty :: CondState
empty = CondState 0 0

isTrue :: CondState -> Bool
isTrue CondState { falseNesting = 0 } = True
isTrue _ = False

isValid :: CondState -> Bool
isValid (CondState x y) = x >= 0 && y >= 0

validate :: String -> CondState -> Either String CondState
validate errMsg condState
  | isValid condState = Right condState
  | otherwise = Left errMsg

nest :: Bool -> CondState -> CondState
nest True condState@CondState{..}
  | falseNesting == 0 =
  condState { trueNesting = trueNesting + 1 }
nest _ condState@CondState{..} =
  condState { falseNesting = falseNesting + 1 }

unnest :: String -> CondState -> Either String CondState
unnest errMsg condState@CondState {..} =
  validate errMsg unnested
  where
    unnested
      | isTrue condState = condState { trueNesting  = trueNesting  - 1 }
      | otherwise        = condState { falseNesting = falseNesting - 1 }

inverse :: String -> CondState -> Either String CondState
inverse errMsg condState =
  nest (not oldIsTrue) <$> unnest errMsg condState
  where
    oldIsTrue = isTrue condState
