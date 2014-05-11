{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lib.Version
  ( version
  ) where

import Data.ByteString.Char8 (ByteString)
import Language.Haskell.TH
import qualified System.Process as Process

version :: ByteString
version = $(stringE . init =<< runIO (Process.readProcess "git" ["rev-parse", "HEAD"] ""))
