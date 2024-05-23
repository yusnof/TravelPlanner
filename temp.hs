import Route
import Graph  as G  -- Create a module and use a sensible graph representation
import Data.PSQueue as PQ
import Data.Map as M
import Data.Maybe (isNothing)
import Prelude as G


type DistanceMap a b = M.Map a b
type PriorityQueue a b = PSQ a b


-- Function to check if a node has not been visited
notVisited :: Ord a => a -> DistanceMap a b -> Bool
notVisited node visitedMap = isNothing (M.lookup node visitedMap)

-- Helper function to unwrap a Maybe value
unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "Attempted to unwrap a Nothing value"

-- Function to get adjacent nodes from the graph
adj :: Ord a => a -> Graph a b -> [(a, b)]
adj node graph = M.findWithDefault [] node graph

-- Dijkstra's algorithm using a recursive helper function
dijkstra :: (Ord a, Ord b, Num b, Ord b) => Graph a b -> a -> DistanceMap a b
dijkstra graph start = dijkstra' initialQueue initialDistances
  where
    initialQueue = PQ.singleton start 0
    initialDistances = M.singleton start 0

    -- Recursive helper function
    dijkstra' :: (Ord a, Ord b, Num b, Ord b) => PriorityQueue a b -> DistanceMap a b -> DistanceMap a b
    dijkstra' queue distMap
      | PQ.null queue = distMap
      | otherwise = dijkstra' newQueue newDistMap
      where
        -- Get the current node with the smallest distance
        currentBinding = unwrap (PQ.findMin queue)
        current = PQ.key currentBinding
        currentDist = PQ.prio currentBinding

        -- Get adjacent nodes that have not been visited
        adjacent = G.adj current graph

        -- Update distances and priority queue
        (newQueue, newDistMap) = G.foldr update (PQ.deleteMin queue, distMap) adjacent

        -- Update function to handle adjacent nodes
        update (neighbor, weight) (q, d) =
          let newDist = currentDist + weight
              oldDist = M.lookup neighbor d
          in case oldDist of
               Just oldDistVal
                 | newDist < oldDistVal -> (PQ.insert neighbor newDist q, M.insert neighbor newDist d)
               Nothing -> (PQ.insert neighbor newDist q, M.insert neighbor newDist d)
               _ -> (q, d)
