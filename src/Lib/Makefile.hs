module Lib.Makefile
  ( module Lib.Makefile.Types
  , module Lib.Makefile.InstantiatePattern
  , parse
  ) where

import Lib.Makefile.InstantiatePattern
import Lib.Makefile.Parser (parse)
import Lib.Makefile.Types
