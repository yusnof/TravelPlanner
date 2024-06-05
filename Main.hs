import Route
import Graph
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe
import System.Environment (getArgs)
import Data.Set as S 

-- Shortest path function
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph from to = Just (reconstructPath fin from to, distance)
    where
     (fin , distance) = dijkstra unvisitedQ visitedSet map graph from to
     map :: Map a (a, b) --map node (prev node, distance to node )
     map = M.empty
     visitedSet :: Set a  --set of all visited nodes
     visitedSet =  S.empty  -- p.queue of all not visited nodes
     unvisitedQ = PQ.empty

-- Dijkstra's algorithm
dijkstra :: (Ord k, Ord p, Num p) => PSQ k p -> Set k -> Map k (k,p) -> Graph k p -> k -> k -> (Map k (k,p), p)
dijkstra queue visitedSet map graph from to = recursive createQueue visitedSet map graph to
    where 
        createQueue = PQ.insert from 0 queue

-- Reconstruct the path from the map
reconstructPath :: (Ord a) => M.Map a (a, b) -> a -> a -> [a]
reconstructPath map start stop 
  | start == stop = [start]
  | isNothing (M.lookup stop map) = error "No path found"
  | otherwise = reconstructPath map start (fst prev) ++ [stop]
  where
    prev = fromJust (M.lookup stop map)

-- Recursive function that finds the shortest path
recursive :: (Ord p, Num p, Ord k) => PSQ k p -> Set k -> Map k (k, p) -> Graph k p -> k -> (Map k (k, p) , p)
recursive queue visitedSet map graph to 
    | PQ.null queue = (map, snd(fromMaybe (error "recursive: no path here ") (M.lookup to map)))
    | currentKey == to = (newMap, currentDist)
    | otherwise = recursive newQ newSet newMap graph to
  where
    (currentKey, currentDist) = (PQ.key (fromJust (PQ.findMin queue)), PQ.prio (fromJust (PQ.findMin queue)))
    adjacent = findAdjacent currentKey visitedSet graph
    newSet = S.insert currentKey visitedSet
    (newQ, newMap) = updateQandMap adjacent currentDist (PQ.deleteMin queue) map currentKey

-- Find all adjacent nodes to the current node
findAdjacent :: (Ord k) => k -> Set k -> Graph k p -> [Edge k p]
findAdjacent currentKey set graph = [x | x <- adj currentKey graph, S.notMember (dst x) set] 

-- Update the priority queue and the map
updateQandMap :: (Ord p, Ord k, Num p) => [Edge k p] -> p -> PSQ k p -> Map k (k,p) -> k -> (PSQ k p, Map k (k,p))
updateQandMap [] _ q map _ = (q, map)
updateQandMap (x:xs) distanceToCurrent q map previous = updateQandMap xs distanceToCurrent newQ newMap previous
  where
    (newQ, newMap) = updateHelp (dst x) (distanceToCurrent + label x) q map previous
    updateHelp :: (Ord p, Ord k) => k -> p -> PSQ k p -> Map k (k, p) -> k -> (PSQ k p, Map k (k,p))
    updateHelp key newValue q map previous = case PQ.lookup key q of
      Nothing -> (PQ.insert key newValue q, M.insert key (previous,newValue) map)
      Just current ->
          if newValue < current
            then (PQ.insert key newValue (PQ.delete key q), M.insert key (previous,newValue) map)
            else (q, map)
    
-- Main function
main :: IO ()
main = do 
  args <- getArgs --Read arguments from console.
  let sFile = args !! 0
  let lFile = args !! 1
  let start = args !! 2
  let end  =  args !! 3
  Right stops <- readStops sFile --Returns [Stops]
  Right lines <- readLines lFile --Returns [Linetables]
  let graph = buildGraph Graph.empty (tableTupleBuilder lines)
  let sPath = fromJust (shortestPath graph start end)
  print $ snd $ sPath
  putStr $ unlines $ fst $ sPath

--tableTupleBuilder builds a list of tuples from a list of linetables
tableTupleBuilder :: [LineTable] -> [(String, Integer)]
tableTupleBuilder table = tup
  where
    lineStops = [stops line | line <- table]
    tup    = [(stopName stops, time stops) | stops <- concat lineStops]

--buildGraph builds a graph from a list of tuples
buildGraph :: (Ord a, Num b, Eq b) => Graph a b -> [(a, b)] -> Graph a b
buildGraph graph []  = graph
buildGraph graph [_] = graph
buildGraph graph ((stop, weight):second@(nextStop, nextWeight):rest)
    | nextWeight == 0 = buildGraph graph (second:rest)
    | otherwise = buildGraph (addEdge stop nextStop nextWeight graph) ((nextStop, nextWeight):rest)