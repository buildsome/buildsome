{-# OPTIONS -Wall -O2 -fno-warn-orphans #-}
module Pretty () where

import Data.ByteString (ByteString)
import Data.Time (DiffTime, NominalDiffTime)
import Buildsome.BuildId (BuildId(..))
import Lib.ColorText (ColorText)
import Lib.FileDesc (FileModeDesc(..), FileStatDesc(..), FileContentDesc(..))
import Lib.StdOutputs(StdOutputs(..))
import Text.PrettyPrint ((<+>), ($+$), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Buildsome.Db as Db
import qualified Data.Map as Map
import qualified Lib.ColorText as ColorText
import qualified Text.PrettyPrint as PP

instance Pretty BuildId where
  pPrint (BuildId i) = pPrint i

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  pPrint m = "{" <+> PP.vcat (map printItem (Map.toList m)) $+$
             "}"
    where
      printItem (k, v) = pPrint k <+> "=>" PP.$$ PP.nest 8 (pPrint v)

instance (Pretty ne, Pretty e) => Pretty (Db.FileDesc ne e) where
  pPrint (Db.FileDescNonExisting x) = "nofile:" <+> pPrint x
  pPrint (Db.FileDescExisting x)    = "exists:" <+> pPrint x

instance Pretty FileModeDesc where
  pPrint (FileModeDesc posixMode) = PP.text (show posixMode)

instance Pretty FileStatDesc where
  pPrint (FileStatDirectory basicStat) = PP.text (show basicStat)
  pPrint (FileStatOther fullStat) = PP.text (show fullStat)

instance Pretty FileContentDesc where
  pPrint (FileContentDescRegular contentHash) =
    "RegularFile (content hash=" <> pPrint contentHash <> ")"
  pPrint (FileContentDescSymlink filePath) = "SymLink to " <> pPrint filePath
  pPrint (FileContentDescDir contentHash) = "Dir (content hash=" <> pPrint contentHash <> ")"

instance Pretty Db.InputDesc where
  pPrint (Db.InputDesc mode stat content) =
    "Input" $+$
    "  " <>
      PP.vcat
      [ "mode = " <+> pPrint mode
      , "stat = " <+> pPrint stat
      , "content = " <+> pPrint content
      ]

instance Pretty ColorText where
  pPrint = PP.text . ColorText.renderStr

pPrintShow :: Show a => a -> PP.Doc
pPrintShow = PP.text . show

instance Pretty NominalDiffTime where pPrint = pPrintShow
instance Pretty DiffTime where pPrint = pPrintShow
instance Pretty ByteString where pPrint = pPrintShow

instance Pretty Db.OutputDesc where
  pPrint (Db.OutputDesc statDesc contentDesc) =
    "Output:" <+> pPrint statDesc <+> pPrint contentDesc

instance Pretty a => Pretty (StdOutputs a) where
  pPrint (StdOutputs out err) = "(stdout=" <> pPrint out <+> "err=" <> pPrint err <> ")"

instance Pretty Db.ExecutionLog where
  pPrint (Db.ExecutionLog buildId cmd inputs outputs stdOuts selfTime) =
    "ExecutionLog" $+$
    "  " <>
    PP.vcat
    [ pPrint buildId
    , pPrint cmd
    , pPrint inputs
    , pPrint outputs
    , pPrint stdOuts
    , "self time = " <> pPrint selfTime
    ]
