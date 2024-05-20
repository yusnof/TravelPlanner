
import Route
import Graph  -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ


--type PrioQ = MinPQueue Edge (Maybe Int)

--To do: 
--Store dequeue from PQ into a list
-- Make alg recursive, currently only "implemented" for the first step.
shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = do -- TODO: implement Dijkstra's algorithm
  PQ.insert from 0 unvisited   -- add start
  
  map help adjacent -- add all adjacent (unvisited) nodes with their distance to Queue.
  
   where
    recursive q g    -- CURRENTLY ADDS NODES TO
      | PQ.null q = []
      | otherwise = do
        let current = PQ.key (PQ.findMin q) -- might be Nothing
        let adjacent = [x | x <- adj current g, notVisited x ]
        PQ.deleteMin unvisited
        recursive (insertAll adjacent unvisited) g

    notVisited (Edge src dst label) | PQ.lookup dst q == Nothing = True
                                    | otherwise = false

    insertAll [] q = q
    insertAll [x] q = insertAll x q
    insertAll ((Edge src dst label):xs) q = insertAll xs (insertIntoQ dst (distanceToStart (Edge src dst label)))

    unvisited:: PSQ Edge Int
    unvisited = PQ.empty
    
    insertIntoQ k v q= PQ.insert k v q
    distanceToStart (Edge scr dest label) = label + PQ.prio (PQ.findMin q)

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
