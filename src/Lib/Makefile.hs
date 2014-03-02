module Lib.Makefile
  ( module Lib.Makefile.Types
  , module Lib.Makefile.InstantiatePattern
  , parseMakefile
  ) where

import Lib.Makefile.InstantiatePattern
import Lib.Makefile.Parser (parseMakefile)
import Lib.Makefile.Types
