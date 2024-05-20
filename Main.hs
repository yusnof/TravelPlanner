
import Route
import Graph  -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ



--type PrioQ = MinPQueue Edge (Maybe Int)

--To do: 
--Store dequeue from PQ into a list
-- Make alg recursive, currently only "implemented" for the first step.
shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = do -- TODO: implement Dijkstra's algorithm
  PQ.insert from (Just 0) unvisited   -- add start
  
  map help adjacent -- add all adjacent (unvisited) nodes with their distance to Queue.
  
   
   where
    recursive q g
      | PQ.null q = []
      | otherwise = do
        let current = PQ.key (PQ.findMin q) -- might be Nothing
        let adjacent = adj current g
      
     
    unvisited:: PSQ Edge (Maybe Int)
    unvisited = PQ.empty
    insertIntoQ k v q= q.insert k v
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

lineToEdge :: [LineTable] -> [Edge String Int]
lineToEdge [] = []
lineToEdge (LineTable _ stops:rest) = stopsToEdges stops ++ lineToEdge rest
  where
    stopsToEdges :: [LineStop] -> [Edge String Int]
    stopsToEdges [] = []
    stopsToEdges [_] = []  
    stopsToEdges (x:y:xs) = Edge (name x) (name y) (timeDifference x y) : stopsToEdges (y:xs)

    timeDifference :: LineStop -> LineStop -> Int
    timeDifference stop1 stop2 = time stop2 - time stop1  


extr :: Stop -> String 
extr (Stop name b) = name

buildGraph :: [Stop] -> [LineTable] -> Graph String b
buildGraph stops lines = addVertices (stopsToString stops) (lineToEdge lines)
