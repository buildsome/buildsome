module Opts (DeleteUnspecifiedOutputs(..), Opt(..), getOpt) where

import Options.Applicative
import Data.Char
import Data.Monoid

data DeleteUnspecifiedOutputs = DeleteUnspecifiedOutputs | DontDeleteUnspecifiedOutputs

deleteUnspecifiedOutputs :: Bool -> DeleteUnspecifiedOutputs
deleteUnspecifiedOutputs False = DontDeleteUnspecifiedOutputs
deleteUnspecifiedOutputs True = DeleteUnspecifiedOutputs

data Opt = Opt { optMakefilePath :: FilePath
               , optParallelism :: Maybe Int
               , optDeleteUnspecifiedOutputs :: DeleteUnspecifiedOutputs
               }

getOpt :: IO Opt
getOpt = execParser opts
  where
    parser = Opt <$> argument str (metavar "MakefilePath")
                 <*> optional
                     (option (short 'j' <>
                              long "parallelism" <>
                              help "How many commands to execute in parallel"))
                 <*> (deleteUnspecifiedOutputs <$>
                      switch (short 'D' <>
                              long "delete-unspecified" <>
                              help "Delete unspecified outputs"))
    opts = info (helper <*> parser) mempty
