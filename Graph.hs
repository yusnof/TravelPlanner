module Graph 
  ( -- * Edge
    Edge                    -- type
  , src, dst, label         -- querying an Edge

    -- * Graph
  , Graph                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges         -- querying a Graph
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source vertex
  , dst   :: a  -- ^ Destination vertex
  , label :: b  -- ^ The label
  } deriving Show

-- A graph with nodes of type a and labels of type b.
newtype Graph a b = Graph (Map a [Edge a b]) 

-- | Create an empty graph
empty :: Graph a b
empty = Graph (M.empty)

-- | Add a vertex (node) to a graph
-- map.insert is o(log n)
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v (Graph m) = Graph (M.insert v [] m) 

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices [] g = g
addVertices [v] g = addVertex v g
addVertices (v:vs) g = addVertices vs (addVertex v g)

--  Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b) and the last one is the graph.
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge srt dest label (Graph m) = Graph (M.insert srt appendEdgeLists m) -- adje
  where appendEdgeLists = [Edge srt dest label] ++ unWrap (M.lookup srt m)
        unWrap (Just x) = x
        unWrap Nothing = [] -- error handling? I dunno should we just crash?
  
-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge src dest label y = addEdge src dest label (addEdge dest src label y)


-- | Get all adjacent vertices (nodes) for a given node
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj k (Graph m)= unWrap(M.lookup k m) --findWithDefault 
  where unWrap (Just x) = x
        unWrap Nothing = []
        
-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices (Graph m) =  map fst (M.toList m) --Så vackert! Det är bara ett Namn! 3 plus points

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges (Graph m) = concatMap snd (M.toList m)