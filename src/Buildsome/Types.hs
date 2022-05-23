{-# LANGUAGE NoImplicitPrelude #-}
module Buildsome.Types where


import           Buildsome.BuildId (BuildId)
import           Buildsome.BuildMaps (BuildMaps(..), TargetRep)
import           Buildsome.Db (Db, Reason)
import           Buildsome.Opts (Opt(..))
import           Buildsome.Slave (Slave)
import           Buildsome.Stats (Stats)
import qualified Control.Exception as E
import           Data.ByteString (ByteString)
import           Data.Set (Set)
import           Lib.ColorText (ColorText)
import           Lib.FSHook (FSHook)
import           Lib.FilePath (FilePath)
import           Lib.Fresh (Fresh)
import           Lib.Makefile (Makefile(..), Target)
import qualified Lib.Parallelism as Parallelism
import           Lib.Printer (Printer)
import qualified Lib.Printer as Printer
import           Lib.SyncMap (SyncMap)

import           Prelude.Compat hiding (FilePath)

type Parents = [(TargetRep, Target, Reason)]

data Buildsome = Buildsome
  { -- static:
    bsOpts :: Opt
  , bsMakefile :: Makefile
  , bsPhoniesSet :: Set FilePath
  , bsBuildId :: BuildId
  , bsRootPath :: FilePath
  , bsBuildMaps :: BuildMaps
    -- dynamic:
  , bsDb :: Db
  , bsFsHook :: FSHook
  , bsSlaveByTargetRep :: SyncMap TargetRep (Parallelism.Entity, Slave Stats)
  , bsFreshPrinterIds :: Fresh Printer.Id
  , bsFastKillBuild :: E.SomeException -> IO ()
  , bsRender :: ColorText -> ByteString
  , bsParPool :: Parallelism.Pool
  }

data WaitOrCancel = Wait | CancelAndWait
  deriving Eq

data PutInputsInStats = PutInputsInStats | Don'tPutInputsInStats
    deriving (Eq, Ord, Show)

data CollectStats = CollectStats PutInputsInStats | Don'tCollectStats
    deriving (Eq, Ord, Show)

data BuildTargetEnv = BuildTargetEnv
  { bteBuildsome :: Buildsome
  , btePrinter :: Printer
  , bteReason :: Reason
  , bteParents :: Parents
  , bteExplicitlyDemanded :: Bool
  , bteSpeculative :: Bool
    -- used by charts & compat makefile, undesirable memory
    -- consumption otherwise
  , bteCollectStats :: CollectStats
  }

data BuiltTargets = BuiltTargets
  { builtTargets :: [Target]
  , builtStats :: Stats
  }
instance Semigroup BuiltTargets where
  (<>) (BuiltTargets a1 b1) (BuiltTargets a2 b2) =
    BuiltTargets (mappend a1 a2) (mappend b1 b2)
instance Monoid BuiltTargets where
  mempty = BuiltTargets mempty mempty
