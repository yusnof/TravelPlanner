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

data Node = String 

-- A graph with nodes of type a and labels of type b.
data Graph a b =  M Node [a String b]  -- TODO: implement a graph with adjacency lists, hint: use a Map.
-- a list ot trepules, (edge vortex) 

-- | Create an empty graph
empty :: Graph a b
empty = undefined

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g = undefined

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = undefined

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l = undefined

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l = undefined

-- | Get all adjacent vertices (nodes) for a given node
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v g = undefined

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices g = undefined

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges g = undefined
