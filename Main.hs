import Route
import Graph   -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe
import System.Environment
import Debug.Trace

-- start prev node
shortestPath :: (Ord a, Ord b, Num b, Show a, Show b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph from to =   Just (findPath set from to graph [], unwrap (M.lookup to set))
    where
     --set:: Map a b
     set       = dijkstra unvisited visited graph from to   
     --visited:: Map a Integer -- set of all visited nodes
     visited   =  M.empty
     --unvisited:: PSQ String Int  -- p.queue of all not visited nodes
     unvisited = PQ.empty

findPath :: (Eq a, Ord a, Ord b, Show a, Show b) => Map a b -> a -> a -> Graph a b -> [a] -> [a]
findPath set start stop graph list
  | start == stop = buildList adjacent
  | otherwise = findPath set newStart stop graph (buildList adjacent)
    where
      buildList [] = list
      buildList (x:xs) = list ++ [src (smallestOfAdjacent (x:xs) set)]
      adjacent = adj start graph
      newStart = dst (smallestOfAdjacent adjacent set)

smallestOfAdjacent :: (Ord b, Ord a, Show a, Show b) => [Edge a b] -> Map a b -> Edge a b 
smallestOfAdjacent [x] set =  x
smallestOfAdjacent (x:xs) set = smallest x (smallestOfAdjacent xs set) 
  where 
    lookup x set = unwrap(M.lookup (src x) set)
    smallest a1 b1 
     | lookup a1 set >= lookup b1 set = b1
     | otherwise = a1   
    

testAdjacent = do
   Right lines <- readLines "lines-gbg.txt" --Returns [Linetables]
   let graph = buildGraph Graph.empty (tableTupleBuilder lines)
   print (Prelude.map show (adj "AAA" graph))
   print (Prelude.map show (adj "BBB" graph))
   print (Prelude.map show (adj "DDD" graph))
   print (Prelude.map show (adj "EEE" graph))


dijkstra :: (Ord k, Ord p, Num p, Show p, Show k) => PSQ k p -> Map k p -> Graph k p -> k -> k -> Map k p
dijkstra queue map graph from to = recursive createQueue map graph to
    where 
        createQueue = PQ.insert from 0 queue

recursive :: (Ord p, Num p, Ord k, Show k, Show p) => PSQ k p -> Map k p -> Graph k p -> k -> Map k p
recursive queue map graph to 
  | PQ.null queue = map
  | otherwise = recursive newQueue newMap graph to 
  where
    currentKey = PQ.key (unwrap2 (PQ.findMin queue))
    currentDist = PQ.prio (unwrap2 (PQ.findMin queue))
    newMap = updateMap currentKey currentDist map
    adjacent = findAdjacent currentKey newMap graph
    newQueue = updateQueue adjacent currentDist (PQ.delete currentKey queue)


findAdjacent :: (Ord k) => k -> Map k p -> Graph k p -> [Edge k p]
findAdjacent currentKey map graph = [x | x <- adj currentKey graph, notVisited x map]

updateMap current dist m = M.insert current dist m
updateQueue [] dist q = q
updateQueue adj dist q = insertAllPQ adj dist q

insertAllPQ :: (Ord a, Num b, Ord b, Show a, Show b) => [Edge a b] -> b -> PSQ a b -> PSQ a b 
insertAllPQ [] _ q = q
insertAllPQ (x:xs) distanceToCurrent q
  | isNothing (PQ.lookup (dst x) q) = insertAllPQ xs distanceToCurrent (PQ.insert (dst x) (distanceToCurrent + label x) q)
  | otherwise =  insertAllPQ xs distanceToCurrent (updatePQvalue (dst x) (distanceToCurrent + label x) q)    


updatePQvalue :: (Ord p, Ord k) => k -> p -> PSQ k p -> PSQ k p
updatePQvalue key newValue q =
  case PQ.lookup key q of
    Nothing -> PQ.insert key newValue q
    Just current ->
      if current >= newValue
        then PQ.insert key newValue (PQ.delete key q)
        else q 

-- unwraps from Maybe a to a            
unwrap :: Maybe a -> a 
unwrap (Just x) = x
unwrap Nothing = error "unwrapp1 nothing"

unwrap2 :: Maybe a -> a 
unwrap2 (Just x) = x
unwrap2 Nothing = error "unwrapp 2 nothing "


-- search if we have been at this node in our map
notVisited :: Ord k => Edge k b -> Map k a -> Bool
notVisited edge map 
       | isNothing (M.lookup (dst edge) map) = True   
       | otherwise = False
{-
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-air.txt" --Returns [Stops]
  Right lines <- readLines "lines-air.txt" --Returns [Linetables]
  let graph = buildGraph Graph.empty lines 
  print (graph)
 -- print (snd(unwrap(shortestPath graph "BOS" "LAX" )))
  print $ "endOfStartGUI"
-}
startGUI :: IO ()
startGUI = do
  args <- getArgs --Read arguments from console.
  let sFile = args !! 0
  let lFile = args !! 1
  let start = args !! 2
  let end  =  args !! 3
  {-
  let sFile = "stops-gbg.txt"
  let lFile = "lines-gbg.txt"
  let start = "AAA"
  let end = "EEE"
  -}
  Right stops <- readStops sFile --Returns [Stops]
  Right lines <- readLines lFile --Returns [Linetables]
  let graph = buildGraph Graph.empty (tableTupleBuilder lines)
  let sPath = unwrap (shortestPath graph start end)
  print "After sPath"
  print "Shortest int"
  print $ fst $ sPath
  print $ "endOfStartGUI"


main :: IO ()
main = do 
  -- startGUI  -- TODO: read arguments, build graph, output shortest path
  args <- getArgs --Read arguments from console.
  let sFile = args !! 0
  let lFile = args !! 1
  let start = args !! 2
  let end  =  args !! 3
  Right stops <- readStops sFile --Returns [Stops]
  Right lines <- readLines lFile --Returns [Linetables]
  let graph = buildGraph Graph.empty (tableTupleBuilder lines)
  let sPath = unwrap (shortestPath graph start end)
  print $ snd (sPath)
  putStr $ unlines $ fst $ sPath




--Since each [LineStops] in each LineTable contains a dummy weight (also called time elsewhere in code) in a tuple as first element  like this: (stop, dummyWeight):()...
-- we can't concatenate all tuple into a single list and then start inserting into graph, we would add weird 0-weight edges between stops that should't have them.
--that is why buildGraph was changed
-- new buildGraph builds seperate [LineStop] for each LineTable and calls insert func on each of them seperately


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
  