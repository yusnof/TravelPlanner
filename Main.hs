import Route
import Graph   -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = do
 PQ.insert from 0 unvisited
 let set = dijkstra unvisited visited g
 return (findShortest set to [], snd(M.lookup to set))
   where
    findShortest set n lst
     | n==to = lst
     | otherwise = do
      let tmp = M.lookup n set
      let adjacent = adj (fst tmp)
      let min = findMin (tail adjacent) (head adjacent)
      findShortest set min (lst++getName min)
     
     
    findMin :: [a] -> b
    findMin [] smallest = smallest
    findMin [x] smallest 
      |snd (M.lookup x set) < snd(M.lookup smallest set) = M.lookup x set
      |otherwise = smallest
    findMin (x:xs) smallest 
      |snd (M.lookup x set) < snd(M.lookup smallest set) = findMin xs (M.lookup x set)
      |otherwise = findMin xs smallest
    {-  -- problemet med det här är att min returnerar en int. men vi vill returnera själva edgen.
    findMin :: [a] -> b
    findMin [] smallest = smallest
    findMin [x] smallest = min (snd(M.lookup x set)) (snd(M.lookup smallest set))
    findMin (x:xs) smallest = findMin xs (min (snd(M.lookup x set)) (snd(M.lookup smallest set)))
   -}
    visited:: Map a (Edge a b ,Int) -- set of all visited nodes
    visited = M.empty
    unvisited:: PSQ (Edge a b)  Int  -- p.queue of all not visited nodes
    unvisited = PQ.empty


dijkstra q m g  -- returns set of all nodes in graph with distances from start node.
      | PQ.null q = m  -- pq is empty, no unvisited nodes. return set of visited
      | otherwise = do
        let current = PQ.key (PQ.findMin q) -- might be Nothing
        let adjacent = [x | x <- adj current g, notVisited x] --build list of adjacent unvisited nodes
        let distanceToCurrent = PQ.prio (PQ.findMin q) -- distance from start to current
        M.insert current (current, distanceToCurrent) m -- add current to visited , key from PQ becoems key in map
        PQ.deleteMin q -- delete current from queue because it is now visited
        dijkstra (insertAllPQ adjacent q) m g -- recursive until done
   where
      notVisited edge 
       | isNothing (M.lookup (getDst edge) m) = true  
       | otherwise = false
      distanceToStart edge = (getLabel edge) + PQ.prio (PQ.findMin q)
      insertAllPQ [] q = q
      insertAllPQ [x] q = insertAllPQ x q
      insertAllPQ (edge:xs) q = insertAllPQ xs (insertIntoQ (getDst edge) (distanceToStart edge))
      insertIntoQ = PQ.insert

main :: IO ()
main = do 
  startGUI  -- TODO: read arguments, build graph, output shortest path
  print $ "endOfMain"

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt" --Returns [Stops]
  Right lines <- readLines "your-lines.txt" --Returns [Linetables]
  let graph = buildGraph stops lines
  print ("sd")
  print $ "endOfStartGUI" 
   


extr :: Stop -> String 
extr (Stop name b) = name

buildGraph :: [Stop] -> [LineTable] -> (Graph String Integer)
<<<<<<< HEAD
buildGraph = undefined 




=======
buildGraph stops = Prelude.foldr addEdges initialGraph   
 where     
  initialGraph = Prelude.foldr (addVertex . name) Empty stops    addEdges (LineTable  stops) 
  graph = Prelude.foldr addStopEdge graph (zip stops (Prelude.drop 1 stops))     
  addStopEdge (LineStop s1 , LineStop s2 t2) 
  -- g = addEdge s1 s2 t2 (addEdge s2 s1 t2 g)
>>>>>>> 98f8504a5768ec0f157389b7104726a8baddcc87
