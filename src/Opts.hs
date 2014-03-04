module Opts (DeleteUnspecifiedOutputs(..), Opt(..), getOpt) where

import Options.Applicative
import Data.Monoid

data DeleteUnspecifiedOutputs = DeleteUnspecifiedOutputs | DontDeleteUnspecifiedOutputs

deleteUnspecifiedOutputs :: Bool -> DeleteUnspecifiedOutputs
deleteUnspecifiedOutputs False = DontDeleteUnspecifiedOutputs
deleteUnspecifiedOutputs True = DeleteUnspecifiedOutputs

data Opt = Opt { optMakefilePath :: FilePath
               , optParallelism :: Maybe Int
               , optGitIgnore :: Bool
               , optDeleteUnspecifiedOutputs :: DeleteUnspecifiedOutputs
               }

opt :: Read a => Mod OptionFields a -> Parser (Maybe a)
opt = optional . option

getOpt :: IO Opt
getOpt = execParser opts
  where
    parser = Opt <$> argument str (metavar "MakefilePath")
                 <*> opt (short 'j' <>
                          long "parallelism" <>
                          help "How many commands to execute in parallel")
                 <*> switch (short 'g' <>
                             long "gitignore" <>
                             help "Write a .gitignore file in the same directory as the Makefile")
                 <*> (deleteUnspecifiedOutputs <$>
                      switch (short 'D' <>
                              long "delete-unspecified" <>
                              help "Delete unspecified outputs"))
    opts = info (helper <*> parser) mempty
