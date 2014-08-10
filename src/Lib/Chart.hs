{-# LANGUAGE CPP, OverloadedStrings #-}
module Lib.Chart
  ( make
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid ((<>))
import Lib.FilePath (FilePath)
import Prelude hiding (FilePath)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.Slave as Slave

#ifdef WITH_CHARTS_SUPPORT

import Control.Monad (void)
import Data.Default.Class (def)
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as ChartCairo

buildTimes :: Slave.Stats -> Chart.PieChart
buildTimes stats =
 def { Chart._pie_data = dataPoints }
 where
  dataPoints =
    let f (targetRep, (_when, count, _deps)) =
          def { Chart._pitem_label = BS8.unpack $ BuildMaps.targetRepPath targetRep
              , Chart._pitem_value = realToFrac count }
    in map f $ M.toList $ Slave.statsOfTarget stats

makePieChart :: Slave.Stats -> FilePath -> IO ()
makePieChart stats filePath = do
  putStrLn $ "Writing chart to " ++ show filePath
  void $ ChartCairo.renderableToFile fileOptions (Chart.toRenderable plot) $ BS8.unpack filePath
  where
    fileOptions = def
      { ChartCairo._fo_format = ChartCairo.SVG
      , ChartCairo._fo_size = (16384, 16384)
      }
    plot = def { Chart._pie_plot = buildTimes stats }

#else

makePieChart :: Slave.Stats -> FilePath -> IO ()
makePieChart _ _ = putStrLn "Re-build buildsome with the Charts flag enabled to get pie charts!"

#endif

type Key = ByteString
data Node = Node
  { nodeDests :: [Key]
  , nodeFontSize :: Double
  }
newtype Graph = Graph { _graph :: Map Key Node }

indent :: ByteString -> ByteString
indent = BS8.unlines . map ("    " <>) . BS8.lines

dotRenderGraph :: ByteString -> Graph -> ByteString
dotRenderGraph graphName (Graph nodes) = BS8.unlines $
  [ "digraph " <> graphName <> " {"
  , indent "rankdir=LR;"
  ] ++
  map (indent . nodeStr) (M.toList nodes) ++
  [ "}" ]
  where
    nodeStr (node, Node dests fontSize) =
      BS8.unlines $
      ( BS8.pack (show node) <> " [fontsize=" <> BS8.pack (show fontSize) <> "];"
      ) :
      [ "  " <>
        BS8.pack (show node) <>
        " -> " <>
        BS8.pack (show dest) <>
        ";"
      | dest <- dests
      ]

statsGraph :: Slave.Stats -> Graph
statsGraph =
  Graph .
  M.map toNode .
  M.mapKeys BuildMaps.targetRepPath .
  M.filter hasDeps .
  Slave.statsOfTarget
  where
    hasDeps (_when, _diffTime, deps) = not $ null deps
    toNode (_when, diffTime, deps) =
      Node
      { nodeDests = map targetAsByteString deps
      , nodeFontSize = 20 + 5 * realToFrac diffTime
      }
    targetAsByteString = BuildMaps.targetRepPath . BuildMaps.computeTargetRep

makeDotChart :: Slave.Stats -> FilePath -> IO ()
makeDotChart stats filePath = do
  putStrLn $ "Writing dot file to " ++ show filePath
  BS8.writeFile (BS8.unpack filePath) $ dotRenderGraph "Dependencies" $ statsGraph stats

make :: Slave.Stats -> FilePath -> IO ()
make stats filePathBase = do
  makePieChart stats (filePathBase <> ".svg")
  makeDotChart stats (filePathBase <> ".dot")
