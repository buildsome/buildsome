{-# LANGUAGE DeriveDataTypeable #-}
module Lib.Makefile.VerifyPhonies
  ( verifyPhonies
  ) where

import qualified Control.Exception as E
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import           Lib.Makefile.Types (Makefile(..), TargetType(..))
import           Lib.Parsec (showPos)
import qualified Text.Parsec.Pos as Pos

data MissingPhony = MissingPhony Pos.SourcePos ByteString deriving (Typeable)
instance E.Exception MissingPhony
instance Show MissingPhony where
  show (MissingPhony pos phony) =
    concat [showPos pos, ": ", ".PHONY refers to inexistent target " ++ BS8.unpack phony]

data PhonyWithCmds = PhonyWithCmds Pos.SourcePos ByteString deriving (Typeable)
instance E.Exception PhonyWithCmds
instance Show PhonyWithCmds where
  show (PhonyWithCmds pos phony) =
    concat [showPos pos, ": ", ".PHONY has cmds " ++ BS8.unpack phony]

verifyPhonies :: Makefile -> IO ()
verifyPhonies makefile =
  case (phoniesWithCmd, invalidPhonies) of
  ([], []) -> return ()
  (_, ((pos, badPhony):_)) -> E.throwIO (MissingPhony pos badPhony)
  (((pos, badPhony):_), _) -> E.throwIO (PhonyWithCmds pos badPhony)
  where
    nonNullCmdTargets = filter (not . BS8.null . targetCmds) $ makefileTargets makefile
    allTargetOutputs = S.fromList $ concatMap targetOutputs $ makefileTargets makefile
    withCmdTargetOutputs = S.fromList $ concatMap targetOutputs nonNullCmdTargets
    phoniesWithCmd = filter ((`S.member` withCmdTargetOutputs) . snd) $ makefilePhonies makefile
    invalidPhonies = filter ((`S.notMember` allTargetOutputs) . snd) $ makefilePhonies makefile
