module Opts (Opt(..), getOpt) where

import Options.Applicative
import Data.Char
import Data.Monoid

data Opt = Opt { optMakefilePath :: FilePath
               , optParallelism :: Maybe Int
               }

getOpt :: IO Opt
getOpt = execParser opts
  where
    parser = Opt <$> argument str (metavar "MakefilePath")
                 <*> optional
                     (option (short 'j' <>
                              long "parallelism" <>
                              help "How many commands to execute in parallel"))
    opts = info (helper <*> parser) mempty
