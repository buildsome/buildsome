{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fforce-recomp #-}
module Lib.Version
  ( version
  ) where


import Prelude.Compat

import Data.ByteString.Char8 (ByteString)
import Language.Haskell.TH
import qualified System.Process as Process
import System.Environment (lookupEnv)

version :: ByteString
version =
  $(stringE =<< runIO (
   do e <- lookupEnv "BUILDSOME_BUILT_REVISION"
      case e of
        Nothing ->
          do sha <- stripWhitespace <$> Process.readProcess "git" ["log", "--pretty=format:%h-%ci", "-1", "HEAD"] ""
             describe <- stripWhitespace <$> Process.readProcess "git" ["describe", "--dirty", "--all"] ""
             return $ sha ++ "-" ++ describe
          where
            stripWhitespace :: String -> String
            stripWhitespace = unwords . words
        Just x -> return x))
