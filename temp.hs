
import Route
import Graph
import Data.PSQueue as PQ





startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-gbg.txt"
  Right lines <- readLines "lines-gbg.txt"
  print (stopsToString stops)
  let graph = buildGraph stops lines
  print $ "endOfStartGUI"