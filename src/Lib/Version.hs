{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}
module Lib.Version
  ( version
  ) where

import Data.ByteString.Char8 (ByteString)
import Language.Haskell.TH
import qualified System.Process as Process
import System.Environment (lookupEnv)

version :: ByteString
version = $(stringE . init =<< runIO (
               do e <- lookupEnv "BUILDSOME_BUILT_REVISION"
                  case e of
                    Nothing -> Process.readProcess "git" ["rev-parse", "HEAD"] ""
                    Just x -> return x))
