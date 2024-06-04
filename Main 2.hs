module Main where

import Route
-- import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import Data.Maybe
import System.Environment (getArgs)
import qualified Data.PSQueue as PSQ
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Ord (comparing)

-------------------------------------------------------------------------------- Shortest Path Algorithm --------------------------------------------------------------------------------
-- Dijkstra's algorithm using a priority queue to keep track of the shortest path
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> S.Set a -> M.Map a b -> M.Map a a -> a -> a -> Maybe ([a], b)
dijkstra graph visited dist prev start end
  | S.null visited = Nothing
  | current == end = Just (reconstructPath start prev end, fromMaybe 0 (M.lookup end dist))
  | otherwise = dijkstra graph newVisited newDist newPrev start end
  where
    current = minimumBy (comparing (dist M.!)) (S.toList visited)
    currentDist = fromMaybe (error "Could not find currentDist") (M.lookup current dist)
    neighbors = adj current graph
    (newDist, newPrev, newVisited) = foldr (update current currentDist) (dist, prev, S.delete current visited) neighbors

-- shortestPath gives us the shortest path between two nodes in a graph
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph start = dijkstra graph (S.singleton start) (M.singleton start 0) M.empty start

--update updates the distance and predecessor maps and the visited set
update :: (Ord a, Ord b, Num b) => a -> b -> Edge a b -> (M.Map a b, M.Map a a, S.Set a) -> (M.Map a b, M.Map a a, S.Set a)
update current currentDist (Edge _ neighbor weight) (d, p, v)
  | tentativeDist < fromMaybe (10^9) (M.lookup neighbor d) = (M.insert neighbor tentativeDist d, M.insert neighbor current p, S.insert neighbor v)
  | otherwise = (d, p, v)
  where tentativeDist = currentDist + weight

--reconstructPath reconstructs the path from the start node to the end node
reconstructPath :: (Ord a) => a -> M.Map a a -> a -> [a]
reconstructPath start prev node
  | node == start = [start]
  | otherwise = case M.lookup node prev of
      Just predecessor -> reconstructPath start prev predecessor ++ [node]
      Nothing -> error "Could not find path"

--------------------------------------------------------------------------------- Main Functions ---------------------------------------------------------------------------------

--main reads the arguments, builds the graph and outputs the shortest path
main :: IO ()
main = do  -- TODO: read arguments, build graph, output shortest path
  args <- getArgs
  let stopsFile = args !! 0
  let linesFile = args !! 1
  let startNode = args !! 2
  let endNode   = args !! 3
  Right lines <- readLines linesFile
  let graph = buildGraph empty $ tableTuple lines
  let path = shortestPath graph startNode endNode
  print $ snd $ fromJust path
  putStr $ unlines $ fst $ fromJust path



--tableTuple creates a list of tuples from a list of LineTables
tableTuple :: [LineTable] -> [(String, Integer)]
tableTuple table = tuples
  where
    lineStops = [stops line | line <- table]
    tuples    = [(stopName stops, time stops) | stops <- concat lineStops]

--buildGraph builds a graph from a list of tuples
buildGraph :: (Ord a, Num b, Eq b) => Graph a b -> [(a, b)] -> Graph a b
buildGraph graph []  = graph
buildGraph graph [_] = graph
buildGraph graph ((stop, weight):snd@(nextStop, nextWeight):rest)
    | nextWeight == 0 = buildGraph graph (snd:rest)
    | otherwise = buildGraph (addEdge stop nextStop nextWeight graph) ((nextStop, nextWeight):rest)


