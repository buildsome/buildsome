{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib.Hash
       ( Hash(..)
       , empty
       , null
       , md5
       ) where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import qualified Crypto.Hash.MD5 as MD5
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           GHC.Generics    (Generic)
import Data.Binary (Binary(..))

import           Prelude.Compat hiding (null)

newtype Hash = Hash { asByteString :: ByteString }
    deriving (Generic, Show, Eq, Ord)
instance Binary Hash
instance NFData Hash where rnf = genericRnf
instance Monoid Hash where
    (Hash x) `mappend` (Hash y) = md5 (x `mappend` y)
    mempty                      = Hash mempty

empty :: Hash
empty = Hash mempty

md5 :: ByteString -> Hash
md5 = Hash . MD5.hash

null :: Hash -> Bool
null = BS8.null . asByteString
