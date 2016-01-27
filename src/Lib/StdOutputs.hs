{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.StdOutputs
  ( StdOutputs(..)
  , str, null
  ) where

import Prelude.Compat hiding (null)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.List (intersperse)
import Data.String (IsString)
import GHC.Generics (Generic)

data StdOutputs a = StdOutputs
  { stdOut :: a
  , stdErr :: a
  } deriving (Generic, Show, Functor, Foldable, Traversable)
instance Binary a => Binary (StdOutputs a)
instance NFData a => NFData (StdOutputs a) where rnf = genericRnf

null :: (Eq a, Monoid a) => StdOutputs a -> Bool
null (StdOutputs out err) = mempty == out && mempty == err

str :: (Eq a, Monoid a, IsString a) => StdOutputs a -> Maybe a
str (StdOutputs out err)
  | mempty == out && mempty == err = Nothing
  | otherwise = Just $ mconcat $ intersperse "\n" $
  [ out | mempty /= out ] ++
  [ err | mempty /= err ]
