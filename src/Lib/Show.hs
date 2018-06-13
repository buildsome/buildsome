module Lib.Show (show) where

import Prelude.Compat hiding (show)
import qualified Prelude.Compat as Prelude

import Data.String (IsString(..))

show :: (Show a, IsString str) => a -> str
show = fromString . Prelude.show
