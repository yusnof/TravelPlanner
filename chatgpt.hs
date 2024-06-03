import Route
import Graph   -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe
import System.Environment
import Debug.Trace

shortestPath :: (Ord a, Ord b, Num b, Show a, Show b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = 
    case M.lookup to set of
        Just distance -> Just (findShortest set from to [from] g, distance)
        Nothing -> Nothing
  where
    set = dijkstra unvisited visited g from  
    visited = M.empty
    unvisited = PQ.singleton from 0

findShortest :: (Eq a, Ord a, Ord b, Show a, Show b) => Map a b -> a -> a -> [a] -> Graph a b -> [a]
findShortest set start stop path graph
  | trace ("findShortest: start = " ++ show start ++ ", stop = " ++ show stop ++ ", path = " ++ show path) False = undefined
  | start == stop = path
  | otherwise = findShortest set newStart stop newPath graph
  where
    adjacent = adj start graph
    newStart = findNext adjacent (head adjacent) set
    newPath = path ++ [newStart]

findNext :: (Ord b, Ord a, Show a,Show b) => [Edge a b] -> Edge a b -> Map a b -> a
findNext [] smallest _ = src smallest
findNext (x:xs) smallest set
  | trace ("findNext: x = " ++ show x ++ ", smallest = " ++ show smallest) False = undefined
  | unwrap (M.lookup (src x) set) < unwrap (M.lookup (src smallest) set) = findNext xs x set
  | otherwise = findNext xs smallest set

dijkstra :: (Ord k, Ord p, Num p, Show k, Show p) => PSQ k p -> Map k p -> Graph k p -> k -> Map k p
dijkstra queue map graph from = dijkstraRec (PQ.insert from 0 queue) map graph

dijkstraRec :: (Ord p, Num p, Ord k, Show k, Show p) => PSQ k p -> Map k p -> Graph k p -> Map k p
dijkstraRec queue map graph
  | PQ.null queue = map
  | otherwise = 
      let current = PQ.key (unwrap (PQ.findMin queue))
          distanceToCurrent = PQ.prio (unwrap (PQ.findMin queue))
          adjacent = Prelude.filter (\x -> notVisited x map) (adj current graph)
          newQueue = insertAllPQ adjacent distanceToCurrent (PQ.deleteMin queue)
          newMap = M.insert current distanceToCurrent map
      in trace ("dijkstraRec: current = " ++ show current ++ ", distanceToCurrent = " ++ show distanceToCurrent ++ ", newQueue = " ++ show newQueue ++ ", newMap = " ++ show newMap) $
         dijkstraRec newQueue newMap graph

insertAllPQ :: (Ord a, Num b, Ord b, Show a, Show b) => [Edge a b] -> b -> PSQ a b -> PSQ a b 
insertAllPQ [] _ q = q
insertAllPQ (x:xs) distanceToCurrent q 
  | trace ("insertAllPQ: x = " ++ show x ++ ", distanceToCurrent = " ++ show distanceToCurrent ++ ", q = " ++ show q) False = undefined
  | isNothing (PQ.lookup (src x) q) = insertAllPQ xs distanceToCurrent (PQ.insert (src x) (distanceToCurrent + label x) q)
  | otherwise = insertAllPQ xs distanceToCurrent (updatePQvalue (src x) (distanceToCurrent + label x) q)    

updatePQvalue :: (Ord p, Ord k,Show k, Show p) => k -> p -> PSQ k p -> PSQ k p
updatePQvalue key newValue q
  | trace ("updatePQvalue: key = " ++ show key ++ ", newValue = " ++ show newValue ++ ", q = " ++ show q) False = undefined
  | unwrap (PQ.lookup key q) < newValue = PQ.insert key newValue (PQ.delete key q)
  | otherwise = q

unwrap :: Maybe a -> a 
unwrap (Just a) = a 
unwrap _ = error "Not find"

notVisited :: Ord k => Edge k b -> Map k a -> Bool
notVisited edge map = isNothing (M.lookup (dst edge) map)

startGUI :: IO ()
startGUI = do
  args <- getArgs
  let sFile = args !! 0
  let lFile = args !! 1
  let start = args !! 2
  let end = args !! 3
  
  Right stops <- readStops sFile
  Right lines <- readLines lFile
  let graph = buildGraph Graph.empty (tableTupleBuilder lines)
  let sPath = shortestPath graph start end
  case sPath of
    Just (path, dist) -> do
      print dist
      putStr $ unlines path
    Nothing -> putStrLn "No path found"
  print "endOfStartGUI"

listToString :: Show a => [a] -> String 
listToString = concatMap show

main :: IO ()
main = startGUI >> print "endOfMain"

tableTupleBuilder :: [LineTable] -> [(String, Integer)]
tableTupleBuilder table = concatMap stopTupleBuilder table

stopTupleBuilder :: LineTable -> [(String, Integer)]
stopTupleBuilder lineTable = [(stopName stop, time stop) | stop <- stops lineTable]

buildGraph :: (Ord a, Num b, Eq b) => Graph a b -> [(a, b)] -> Graph a b
buildGraph graph [] = graph
buildGraph graph [_] = graph
buildGraph graph ((stop, weight):(nextStop, nextWeight):rest)
  | nextWeight == 0 = buildGraph graph ((nextStop, nextWeight):rest)
  | otherwise = buildGraph (addEdge stop nextStop nextWeight graph) ((nextStop, nextWeight):rest)
