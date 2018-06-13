{-# LANGUAGE CPP #-}
module Buildsome.Chart
  ( make
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.Stats (Stats(..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid ((<>))
import Lib.FilePath (FilePath)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Csv as Csv
import qualified Data.Map as M

#ifdef WITH_CHARTS_SUPPORT

import Control.Monad (void)
import Data.Default.Class (def)
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as ChartCairo

buildTimes :: Stats -> Chart.PieChart
buildTimes stats =
 def { Chart._pie_data = dataPoints }
 where
  dataPoints =
    let f (targetRep, targetStats) =
          def { Chart._pitem_label = BS8.unpack $ BuildMaps.targetRepPath targetRep
              , Chart._pitem_value = realToFrac (Stats.tsTime targetStats) }
    in map f $ M.toList $ Stats.ofTarget stats

makePieChart :: Stats -> FilePath -> IO ()
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

makePieChart :: Stats -> FilePath -> IO ()
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

statsGraph :: Stats -> Graph
statsGraph =
  Graph .
  M.map toNode .
  M.mapKeys BuildMaps.targetRepPath .
  M.filter hasDeps .
  Stats.ofTarget
  where
    hasDeps targetStats = not $ null $ Stats.tsDirectDeps targetStats
    toNode targetStats =
      Node
      { nodeDests = map targetAsByteString $ Stats.tsDirectDeps targetStats
      , nodeFontSize = 20 + 5 * realToFrac (Stats.tsTime targetStats)
      }
    targetAsByteString = BuildMaps.targetRepPath . BuildMaps.computeTargetRep

makeDotChart :: Stats -> FilePath -> IO ()
makeDotChart stats filePath = do
  putStrLn $ "Writing dot file to " ++ show filePath
  BS8.writeFile (BS8.unpack filePath) $ dotRenderGraph "Dependencies" $ statsGraph stats


toCSV :: Stats -> ByteString
toCSV stats = LBS8.toStrict . Csv.encode . map toRow . M.toList $ ofTarget stats
  where
    toRow (targetRep, targetStats) = 
      ( show targetRep
      , show $ Stats.tsWhen targetStats
      , (truncate $ 1000 * Stats.tsTime targetStats) :: Integer
      )

makeCSVFile :: Stats -> FilePath -> IO ()
makeCSVFile stats filePath = do
  putStrLn $ "Writing csv file to " ++ show filePath
  BS8.writeFile (BS8.unpack filePath) $ toCSV stats
    
    
make :: Stats -> FilePath -> IO ()
make stats filePathBase = do
  makePieChart stats (filePathBase <> ".svg")
  makeDotChart stats (filePathBase <> ".dot")
  makeCSVFile stats (filePathBase <> ".csv")
  
