
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
  --let graph = addVertices [stops.fst] (addEdge extract lines) -- TODO: build your graph here using stops and lines
  print $ "endOfStartGUI" 
  
--extract :: LineTable -> Edge a b 
--extract (LineTable _ (x:y:xs)) = addEdge x y exName(x) empty

exName (LineStop name time) = name
   