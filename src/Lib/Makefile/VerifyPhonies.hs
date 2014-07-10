{-# LANGUAGE DeriveDataTypeable #-}
module Lib.Makefile.VerifyPhonies
  ( verifyPhonies
  ) where

import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Lib.Parsec (showPos)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import qualified Text.Parsec.Pos as Pos
import Lib.Makefile.Types (Makefile(..), TargetType(..))

data MissingPhony = MissingPhony Pos.SourcePos ByteString deriving (Typeable)
instance E.Exception MissingPhony
instance Show MissingPhony where
  show (MissingPhony pos danglingInput) =
    concat [showPos pos, ": ", ".PHONY refers to inexistent target " ++ BS8.unpack danglingInput]

verifyPhonies :: Makefile -> IO ()
verifyPhonies makefile =
  case filter ((`S.notMember` outputPathsSet) . snd) (makefilePhonies makefile) of
  [] -> return ()
  ((pos, danglingInput):_) -> E.throwIO (MissingPhony pos danglingInput)
  where
    outputPathsSet = S.fromList $ concatMap targetOutputs $ makefileTargets makefile
