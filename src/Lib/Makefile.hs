module Lib.Makefile
  ( module Lib.Makefile.Types
  , module Lib.Makefile.InstantiatePattern
  , module Lib.Makefile.VerifyPhonies
  , Parser.parse
  ) where

import Lib.Makefile.Types
import Lib.Makefile.InstantiatePattern
import Lib.Makefile.VerifyPhonies
import qualified Lib.Makefile.Parser as Parser
