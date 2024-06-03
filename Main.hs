import Route
import Graph   -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe
import System.Environment
import Debug.Trace

shortestPath :: (Ord a, Ord b, Num b, Show a, Show b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = Just (findShortest set from to [] g, unwrap (M.lookup to set))
    where
     --set:: Map a b
     set = dijkstra unvisited visited g from  
     --visited:: Map a Integer -- set of all visited nodes
     visited = M.empty
     --unvisited:: PSQ String Int  -- p.queue of all not visited nodes
     unvisited = PQ.empty
 
findShortest :: (Eq a, Ord a, Ord b) => Map a b -> a -> a -> [a]-> Graph a b -> [a]
findShortest set start stop list graph
 | start == stop = list
 | otherwise = findShortest set newstart stop newlist graph
 where
    newstart = findNext (tail adjacent) (head adjacent) set
    adjacent = adj start graph
    newlist  = list ++ [newstart]
  
-- if adjacent is singleton (node only has 1 connected edge) what will happen? tail [x] = [], M.lookup [] ??
findNext :: (Ord b, Ord a) => [Edge a b] -> Edge a b -> Map a b -> a
findNext [] smallest set = src smallest
--findNext [x] smallest set
-- |  unwrap(M.lookup (src x) set)  <  unwrap(M.lookup (src smallest) set) = src smallest
-- |  otherwise = src smallest
findNext (x:xs) smallest set
  | unwrap(M.lookup (src x) set) > unwrap(M.lookup (src smallest) set) = findNext xs x set
  | otherwise = findNext xs smallest set

dijkstra :: (Ord k, Ord p, Num p, Show p, Show k) => PSQ k p -> Map k p -> Graph k p -> k -> Map k p
dijkstra queue map graph from =  recursive createQueue map graph
    where 
        createQueue = PQ.insert from 0 queue

recursive :: (Ord p, Num p, Ord k, Show p, Show k) => PSQ k p -> Map k p -> Graph k p -> Map k p
recursive queue map graph 
  -- | trace ("insertAllPQ: x = " ++ show queue ++ ", distanceToCurrent = " ++ show map) False= undefined
 | PQ.null queue = map
 | trace ("adj" ++ show adjacent) False  = undefined
 | otherwise = recursive newqueue newmap graph
  where
    current = PQ.key (unwrap (PQ.findMin queue)) 
    adjacent = [x | x <- adj current graph, notVisited x map]  -- adj takes in key
    newqueue = insertAllPQ adjacent distanceToCurrent (PQ.deleteMin queue)
    distanceToCurrent = PQ.prio (unwrap (PQ.findMin queue))
    newmap = M.insert current distanceToCurrent map

insertAllPQ :: (Ord a, Num b, Ord b, Show a, Show b) => [Edge a b] -> b -> PSQ a b -> PSQ a b 
insertAllPQ [] _ q = q
insertAllPQ [x] distanceToCurrent q 
  -- | trace ("insertAllPQ: x = " ++ show x ++ ", distanceToCurrent = " ++ show distanceToCurrent ++ ", q = " ++ show q) False = undefined
  | isNothing (PQ.lookup (src x) q) = PQ.insert (src x) (distanceToCurrent + label x) q
  | otherwise = updatePQvalue (src x) (distanceToCurrent + label x) q
insertAllPQ (x:xs) distanceToCurrent q
  | isNothing (PQ.lookup (src x) q) = insertAllPQ xs distanceToCurrent (PQ.insert (src x) (distanceToCurrent + label x) q)
  | otherwise =  insertAllPQ xs distanceToCurrent (updatePQvalue (src x) (distanceToCurrent + label x) q)    


--updatePQvalue :: (Ord p, Ord k) => String -> Integer -> PSQ k p -> PSQ k p
updatePQvalue :: (Ord p, Ord k) => k -> p -> PSQ k p -> PSQ k p
updatePQvalue key newValue q
 | unwrap(PQ.lookup key q) < newValue = update key newValue q
 | otherwise = q 
  where 
    update key newValue q = PQ.insert key newValue (PQ.delete key q) 


-- unwraps from Maybe a to a            
unwrap :: Maybe a -> a 
unwrap (Just a) = a 
unwrap _ = error "Not find"

-- search if we have been at this node in our map
notVisited :: Ord k => Edge k b -> Map k a -> Bool
notVisited edge map 
       | isNothing (M.lookup (dst edge) map) = True   
       | otherwise = False
{-}
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
  
  
  Right stops <- readStops sFile --Returns [Stops]
  Right lines <- readLines lFile --Returns [Linetables]
  let graph = buildGraph Graph.empty (tableTupleBuilder lines)
  let sPath = (shortestPath graph start end)
  print $ snd $ fromJust sPath
  putStr $ unlines $ fst $ fromJust sPath
  
  --print ("After sPath")
  --print (listToString sPath)
  print $ "endOfStartGUI"

listToString [] = [] 
listToString (x:xs) =  show x ++ listToString xs

main :: IO ()
main = do 
  startGUI  -- TODO: read arguments, build graph, output shortest path
  print $ "endOfMain"

{-
stopTupleBuilder ::  [LineStop] -> [(String, Integer)]
stopTupleBuilder stops = [(stopName x, time x) | x <- stops]

buildGraph :: Graph String Integer -> [LineTable] -> Graph String Integer
buildGraph graph [] = graph
buildGraph graph [x] = help graph (stopTupleBuilder (stops x))
buildGraph graph (x:xs) = buildGraph (help graph  (stopTupleBuilder (stops x))) xs

help ::  Graph String Integer -> [(String, Integer)] -> Graph String Integer
help graph [] = graph
help graph [(stop,weight):(nextStop,nextWeight)] = addEdge stop nextStop nextWeight graph
help graph ((stop,weight):(nextStop,nextWeight):rest)  = help (addEdge stop nextStop nextWeight graph) ((nextStop, nextWeight):rest)
-}

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
  

