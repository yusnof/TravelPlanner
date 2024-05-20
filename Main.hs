
import Route
import Graph  -- Create a module and use a sensible graph representation
--import Data.PQueue.Min -- as PQ
import Data.PSQueue as PQ



--type PrioQ = MinPQueue Edge (Maybe Int)

--To do: 
--Store dequeue from PQ into a list
-- Make alg recursive, currently only "implemented" for the first step.
shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = do -- TODO: implement Dijkstra's algorithm
  visited = visited ++ insert from (Just 0) -- add start
  let adjacent = adj from g -- get adjacent
   
  map help adjacent -- add all adjacent (unvisited) nodes with their distance to Queue.
  
   
   where
    visited = []
    unvisited:: PSQueue Edge (Maybe Int)
    unvisited = PQ.empty
    help  (Edge src dst label) = 

main :: IO ()
main = do 
  startGUI  -- TODO: read arguments, build graph, output shortest path
  print $ "endOfMain"

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = buildGraph stops lines  
  print $ "endOfStartGUI" 
  
stopsToString ::  [Stop] -> [String]
stopsToString [x] = [extr x]
stopsToString (x:xs) = extr x : stopsToString xs  

extr :: Stop -> String 
extr (Stop name b) = name

buildGraph :: [Stop] -> [LineTable] -> Graph String b
buildGraph stops lines = addVertices (stopsToString stops) Graph.empty
