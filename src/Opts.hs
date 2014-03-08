module Opts (DeleteUnspecifiedOutputs(..), Opt(..), getOpt) where

import Options.Applicative
import Data.Monoid

data DeleteUnspecifiedOutputs = DeleteUnspecifiedOutputs | DontDeleteUnspecifiedOutputs

deleteUnspecifiedOutputs :: Bool -> DeleteUnspecifiedOutputs
deleteUnspecifiedOutputs False = DontDeleteUnspecifiedOutputs
deleteUnspecifiedOutputs True = DeleteUnspecifiedOutputs

data Opt = Opt { optMakefilePath :: Maybe FilePath
               , optParallelism :: Maybe Int
               , optGitIgnore :: Bool
               , optDeleteUnspecifiedOutputs :: DeleteUnspecifiedOutputs
               }

opt :: Read a => Mod OptionFields a -> Parser (Maybe a)
opt = optional . option

strOpt :: Mod OptionFields String -> Parser (Maybe String)
strOpt = optional . strOption

getOpt :: IO Opt
getOpt = execParser opts
  where
    parser = Opt <$> strOpt (short 'f' <>
                             long "file" <>
                             metavar "file" <>
                             help "Use file as a makefile.")
                 <*> opt (short 'j' <>
                          long "parallelism" <>
                          help "How many commands to execute in parallel" <>
                          metavar "jobs")
                 <*> switch (short 'g' <>
                             long "gitignore" <>
                             metavar "path" <>
                             help "Write a .gitignore file in the same directory as the Makefile")
                 <*> (deleteUnspecifiedOutputs <$>
                      switch (short 'D' <>
                              long "delete-unspecified" <>
                              help "Delete unspecified outputs"))
    opts = info (helper <*> parser) mempty
