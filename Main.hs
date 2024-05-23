import Route
import Graph   -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe (isNothing)

--shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = Just (findShortest set from to [], M.lookup to set)
    where
     set = dijkstra unvisited visited g from  
     --visited:: Map a (Edge a b, Integer) -- set of all visited nodes
     visited = M.empty
     --unvisited:: PSQ String Int  -- p.queue of all not visited nodes
     unvisited = PQ.empty
 
--findShortest :: Map a (Edge a b, Int) -> String -> String -> [String]-> Graph String b -> [String]
findShortest set start stop list graph
 | start == stop = list
 | otherwise = findShortest set newstart stop newlist graph
 where
    newstart = findNext (tail adjacent) (head adjacent) set
    adjacent = adj start graph
    newlist  = list++newstart
  
-- if adjacent is singleton (node only has 1 connected edge) what will happen? tail [x] = [], M.lookup [] ??
findNext :: (Ord k, Ord (Edge k b1)) => [Edge k b2] -> Edge k b1 -> Map k (a, Edge k b1) -> k
findNext [] smallest set = getName smallest --getName smallest
findNext [x] smallest set
 |  snd (unwrap(M.lookup (getName x) set))  <  snd ( unwrap(M.lookup (getName smallest) set)) = getName smallest
 | otherwise = getName smallest
findNext (x:xs) smallest set   
  | snd (unwrap(M.lookup (getName x) set)) > snd(unwrap(M.lookup (getName smallest) set)) = findNext xs (snd (unwrap(M.lookup (getName x) set))) set
  | otherwise = findNext xs smallest set

dijkstra :: (Ord k, Ord p, Num p) => PSQ k p -> Map k ([Edge k p], p) -> Graph k p -> k -> Map k ([Edge k p], p)
dijkstra queue map graph from = recursive help map graph
    where 
        help = PQ.insert from 0 queue

recursive :: (Ord p, Num p, Ord k) => PSQ k p -> Map k ([Edge k p], p) -> Graph k p -> Map k ([Edge k p], p)
recursive queue map graph 
 | PQ.null queue = map
 | otherwise = recursive newqueue newmap graph
  where
    current =  PQ.key (unwrap (PQ.findMin queue))
    adjacent = [ x | x <- adj current graph, notVisited x map]  -- adj takes in key
    newqueue = insertAllPQ adjacent distanceToCurrent (PQ.deleteMin queue)
    distanceToCurrent = PQ.prio (unwrap (PQ.findMin queue))
    newmap = M.insert current (graphLookUp current graph,distanceToCurrent) map
    insertAllPQ [] _ q = q
    insertAllPQ [x] distanceToCurrent q = PQ.insert (getName x) (distanceToCurrent + getLabel x) q
    insertAllPQ (x:xs) distanceToCurrent q = insertAllPQ xs distanceToCurrent (PQ.insert (getName x) (distanceToCurrent + getLabel x) q)

-- unwraps from Maybe a to a            
unwrap :: Maybe a -> a 
unwrap (Just a) = a 
unwrap _ = error "Not find"

-- search if we have been at this node in our map
notVisited :: Ord k => Edge k b -> Map k a -> Bool
notVisited edge map 
       | isNothing (M.lookup (getDst edge) map) = True  
       | otherwise = False

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-air.txt" --Returns [Stops]
  Right lines <- readLines "lines-air.txt" --Returns [Linetables]
  let graph = buildGraph Graph.empty (tableTupleBuilder lines) 
  print (graph)
  --print (snd(shortestPath graph "BOS" "LAX" ))
  print $ "endOfStartGUI"

main :: IO ()
main = do 
  startGUI  -- TODO: read arguments, build graph, output shortest path
  print $ "endOfMain"

--tableTuple creates a list of tuples from a list of LineTables
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





