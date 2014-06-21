module Lib.Makefile
  ( module Lib.Makefile.Types
  , module Lib.Makefile.InstantiatePattern
  , parse
  , Vars
  ) where

import Lib.Makefile.InstantiatePattern
import Lib.Makefile.Parser (parse, Vars)
import Lib.Makefile.Types
