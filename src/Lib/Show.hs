module Lib.Show (show) where

import Data.String (IsString(..))
import Prelude hiding (show)
import qualified Prelude

show :: (Show a, IsString str) => a -> str
show = fromString . Prelude.show
