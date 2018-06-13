{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.Posix.Instances () where

import Data.Binary(Binary)
import Foreign.C.Types (CTime(..))
import GHC.Generics (Generic)
import System.Posix.ByteString (COff(..), CNlink(..), CUid(..), CGid(..), CIno(..), CDev(..))

deriving instance Generic CTime
instance Binary CTime
deriving instance Generic COff
instance Binary COff
deriving instance Generic CNlink
instance Binary CNlink
deriving instance Generic CUid
instance Binary CUid
deriving instance Generic CGid
instance Binary CGid
deriving instance Generic CIno
instance Binary CIno
deriving instance Generic CDev
instance Binary CDev
