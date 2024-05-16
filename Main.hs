
import Route
-- import RouteGUI
import Graph  -- Create a module and use a sensible graph representation



shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined -- TODO: implement Dijkstra's algorithm

main :: IO ()
main = do 
  startGUI  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  print $ "endOfStartGUI" 
  --
  --runGUI stops lines graph shortestPath