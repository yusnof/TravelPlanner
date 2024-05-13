{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-|
Module      : RouteGUI
Description : Graphical interface for the path finding lab
Copyright   : (c) Anton Ekblad, Alex Gerdes, 2019
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
Portability :  -
-}

module RouteGUI 
  ( -- * Types
    Name, Cost, ShortestPath
    -- * Run the server 
  , runGUI
  ) where

import qualified Graphics.UI.Threepenny as UI
import qualified Data.Map as M
import Graphics.UI.Threepenny.Core
import Control.Monad
import Route as Route

-- | The cost of a path, expressed in minutes.
type Cost = Integer

-- | The name of a stop.
type Name = String

-- | A function to calculate the shortest path between two stops in a graph of
-- type @g@. Returns the list of stops making up that path and the total cost of
-- the path. Must return @Nothing@ if no path could be found.
type ShortestPath g = g -> Name -> Name -> Maybe ([Name], Cost)

-- | Get the proper color for the given line. Other lines are a stylish shade of
-- pink.
lineColor :: Integer -> Color
lineColor 1  = RGB 0 0 0 -- TODO: better color for #1
lineColor 2  = RGB 255 255 0
lineColor 3  = RGB 0 0 127
lineColor 4  = RGB 0 127 0
lineColor 5  = RGB 255 0 0
lineColor 6  = RGB 255 255 0
lineColor 7  = RGB 127 63 63
lineColor 8  = RGB 127 0 63
lineColor 9  = RGB 223 0 255
lineColor 10 = RGB 0 255 0
lineColor 11 = RGB 0 0 0
lineColor 13 = RGB 255 63 63
lineColor _  = RGBA 255 0 224 0.2

-- | Launch a travel planner GUI using a custom shortest path implementation.
--   The GUI can be reached by pointing a web browser at http://localhost:8888.
runGUI :: [Stop]       -- ^ A list of all list of nodes in the graph.
       -> [LineTable]  -- ^ All lines (edges in the graph).
       -> graph        -- ^ A value representing the graph; naming this type Graph
                       --   or similar is probably a good idea.
       -> (graph -> Name -> Name -> Maybe ([Name], Cost))
                       -- ^ A function taking a graph, a start node and an end
                       --   node, producing the list of nodes making up the
                       --   shortest path from start to end and the total cost of
                       --   that path.
       -> IO ()
runGUI stops lines graph pathfun = do
  -- Helpful message for those who are lost
  putStrLn "\n\n"
  putStrLn $ replicate 78 '='
  putStrLn $ '|' : replicate 76 ' ' ++ "|"
  putStrLn "| Point your browser to http://localhost:8888 to see your program in action! |"
  putStrLn $ '|' : replicate 76 ' ' ++ "|"
  putStrLn $ replicate 78 '='
  putStrLn "\n\n"

  startGUI defaultConfig { jsPort = Just 8888 } $ \s -> 
    guiMain s stops lines graph pathfun

guiMain :: Window -> [Stop] -> [LineTable] -> g -> ShortestPath g -> UI ()
guiMain w stops lines graph findpath = do
  let stopnames = map name stops
      stopmap = M.fromList $ zip stopnames stops
  -- Just layout the elements
  -- CSS is directly in the code, since 3p makes it kind of painful to do it
  -- properly.
  btn <- UI.button # UI.set UI.text "Find path"
                   # UI.set UI.style [("position", "absolute"),
                                      ("margin-left", "5em")]
  stoplist <- UI.div # UI.set UI.id_ "pathlist"
                     # UI.set UI.style [("border", "1px solid black"),
                                        ("width", "500px"),
                                        ("height", "200px"),
                                        ("padding", "0.5em"),
                                        ("overflow", "auto")]
  can <- UI.canvas # UI.set UI.id_ "canvas"
                   # UI.set UI.style [("border", "1px solid black"),
                                      ("top", "1em"),
                                      ("left", "550px"),
                                      ("position", "absolute")]
                   # UI.set UI.width 700
                   # UI.set UI.height 620

  -- Buffer to contain pre-rendered map, since the roundtrip time to draw it all
  -- is terrible.
  buf <- UI.canvas # UI.set UI.id_ "buffer"
                   # UI.set UI.style [("display", "none")]
                   # UI.set UI.width 700
                   # UI.set UI.height 620

  -- To/from list boxes
  fromlist <- listBox "from" stopnames
  tolist <- listBox "to" stopnames
  fromTo <- UI.row [UI.string "From" # UI.set UI.style [("padding","0.5em")],
                    element fromlist,
                    UI.string "to" # UI.set UI.style [("padding","0.5em")],
                    element tolist]

  -- Total travel time label
  traveltime <- UI.div # UI.set UI.style [("padding-top", "0.5em")]

  -- All the interactive pieces
  controls <- UI.row [element fromTo, element btn]
  element controls # UI.set UI.style [("padding-bottom", "0.5em")]

  -- All the visualisation stuff
  pathviz <- UI.column [element stoplist, element traveltime]
  ui <- UI.column [element controls, element pathviz]
  UI.getBody w #+ [element ui, element can, element buf]

  -- onclick handler for the path button
  UI.on UI.click btn $ \_ -> do
    mfrom <- UI.get UI.selection fromlist
    mto <- UI.get UI.selection tolist
    case (mfrom, mto) of
      (Just from, Just to) -> do
        case findpath graph (stopnames !! from) (stopnames !! to) of
          Just (p, c) -> do
            -- Path found; add all stops to path box and render.
            kids <- forM p $ \stop -> do
              UI.p # UI.set UI.html stop
                   # UI.set UI.style [("padding", "0.1em"),
                                      ("margin", "0.1em")]
            UI.element stoplist # UI.set UI.children kids
            UI.element traveltime # UI.set UI.text (showCost c)
            drawPath buf can stopmap p
          _ -> do
            UI.element traveltime # UI.set UI.text noSuchPath
            drawPath buf can stopmap []
      _ ->
        return ()

  -- Draw initial map without traveled paths
  drawMap stopmap lines
  drawPath buf can stopmap []
  return ()

drawMap :: M.Map Name Stop -> [LineTable] -> UI ()
drawMap stopmap lines =
  render "buffer" $ scale (0.6, 0.6)
                  $ textStyle "normal 14pt arial" $ do
    forM_ (M.elems stopmap) $ \(Stop n (x, y)) -> do
      RouteGUI.text (fromInteger x+5, 1000-fromInteger y+20) n
    forM_ lines $ \l -> do
      let ss = [fi $ position (stopmap M.! (stopName s)) | s <- Route.stops l]
      color (lineColor (fromInteger $ lineNumber l)) $ stroke $ path ss
  where
    fi = from2Integers

from2Integers :: (Integer, Integer) -> (Int, Int)
from2Integers (x, y) = (fromInteger x, 1000-fromInteger y)

drawPath :: UI.Canvas -> UI.Canvas -> M.Map Name Stop -> [Name] -> UI ()
drawPath buf can stopmap p = do
  UI.clearCanvas can
  UI.drawImage buf (0, 0) can
  let p' = map (from2Integers . position . (stopmap M.!)) p
  renderOnTop "canvas" $ scale (0.6, 0.6)
                       $ lineWidth 10
                       $ color (RGBA 0 0 0 0.2)
                       $ stroke $ path p'

showCost :: Cost -> String
showCost c = "Total travel time: " ++ show c ++ " minutes"

noSuchPath :: String
noSuchPath = "No such path!"

listBox :: String -> [String] -> UI Element
listBox ident elems = do
  elems' <- forM elems $ \n -> do
    mkElement "option" # UI.set UI.html n
                       # UI.set UI.value n
  UI.mkElement "select" # UI.set UI.children elems'
                        # UI.set UI.id_ ident

-- Canvas stuff, since 3p's canvas facilities are sorely lacking.
type Point = (Int, Int)
type Vector = (Double, Double)
newtype Picture a = Picture {unP :: String -> UI a}
newtype Shape a = Shape {unS :: String -> UI a}

jsBeginPath :: String -> JSFunction ()
jsBeginPath = ffi "document.getElementById(%1).getContext('2d').beginPath(); null;"

jsMoveTo :: String -> Int -> Int -> JSFunction ()
jsMoveTo = ffi "document.getElementById(%1).getContext('2d').moveTo(%2, %3); null;"

jsLineTo :: String -> Int -> Int -> JSFunction ()
jsLineTo = ffi "document.getElementById(%1).getContext('2d').lineTo(%2, %3); null;"

jsStroke :: String -> JSFunction ()
jsStroke = ffi "document.getElementById(%1).getContext('2d').stroke(); null;"

jsScale :: String -> Double -> Double -> JSFunction ()
jsScale eid x y =
  ffi "document.getElementById(%1).getContext('2d').scale(%2, %3); null;"
      eid
      (show x)
      (show y)

jsPushState :: String -> JSFunction ()
jsPushState = ffi "document.getElementById(%1).getContext('2d').save(); null;"

jsPopState :: String -> JSFunction ()
jsPopState = ffi "document.getElementById(%1).getContext('2d').restore(); null;"

jsResetCanvas :: String -> JSFunction ()
jsResetCanvas = ffi "document.getElementById(%1).width = document.getElementById(%1).width; null;"

jsSetProp :: String -> String -> String -> JSFunction ()
jsSetProp = ffi "document.getElementById(%1).getContext('2d')[%2] = %3; null;"

jsDrawText :: String -> String -> Int -> Int -> JSFunction ()
jsDrawText = ffi "document.getElementById(%1).getContext('2d').fillText(%2, %3, %4); null;"

jsTextStyle :: String -> String -> JSFunction ()
jsTextStyle = flip jsSetProp "font"

-- | A color, specified using its red, green and blue components, with an
--   optional alpha component.
data Color = RGB Int Int Int
           | RGBA Int Int Int Double

c2s :: Color -> String
c2s (RGB r g b) =
  concat ["rgb(", show r, ",", show g, ",", show b, ")"]
c2s (RGBA r g b a) =
  concat ["rgba(", show r, ",", show g, ",", show b, ",", show a, ")"]

instance Functor Picture where
        fmap f p = Picture $ \ctx -> unP p ctx >>= return . f

instance Applicative Picture where
    pure a = Picture $ \_ -> return a

    pfab <*> pa = Picture $ \ctx -> do
        fab <- unP pfab ctx
        a   <- unP pa   ctx
        return (fab a)

instance Monad Picture where
  return x = Picture $ \_ -> return x
  Picture m >>= f = Picture $ \ctx -> do
    x <- m ctx
    unP (f x) ctx

instance Functor Shape where
    fmap f s = Shape $ \ctx ->
        unS s ctx >>= return . f

instance Applicative Shape where
    pure a = Shape $ \_ -> return a

    sfab <*> sa = Shape $ \ctx -> do
        fab <- unS sfab ctx
        a   <- unS sa   ctx
        return (fab a)

instance Monad Shape where
  return x = Shape $ \_ -> return x
  Shape m >>= f = Shape $ \ctx -> do
    x <- m ctx
    unS (f x) ctx

-- | Clear a canvas, then draw a picture onto it.
render :: String -> Picture () -> UI ()
render elemid (Picture p) = do
  callFunction $ jsResetCanvas elemid
  p elemid

-- | Draw a picture onto a canvas without clearing it.
renderOnTop :: String -> Picture () -> UI ()
renderOnTop elemid (Picture p) = p elemid

color :: Color -> Picture () -> Picture ()
color c (Picture pict) = Picture $ \ctx -> do
  callFunction $ jsPushState ctx
  callFunction $ jsSetProp ctx "strokeStyle" (c2s c)
  pict ctx
  callFunction $ jsPopState ctx

lineWidth :: Int -> Picture () -> Picture ()
lineWidth w (Picture pict) = Picture $ \ctx -> do
  callFunction $ jsPushState ctx
  callFunction $ jsSetProp ctx "lineWidth" (show w)
  pict ctx
  callFunction $ jsPopState ctx

-- | Draw the specified picture scaled as specified by the scale vector.
scale :: Vector -> Picture () -> Picture ()
scale (x, y) (Picture pict) = Picture $ \ctx -> do
  callFunction $ jsPushState ctx
  callFunction $ jsScale ctx x y
  pict ctx
  callFunction $ jsPopState ctx
  
-- | Draw the contours of a shape.
stroke :: Shape () -> Picture ()
stroke (Shape shape) = Picture $ \ctx -> do
  callFunction $ jsBeginPath ctx
  shape ctx
  callFunction $ jsStroke ctx

-- | Draw a path along the specified points.
path :: [Point] -> Shape ()
path ((x1, y1):ps) = Shape $ \ctx -> do
    callFunction $ jsMoveTo ctx x1 y1
    mapM_ (lineto ctx) ps
  where
    lineto c (a, b) = callFunction $ jsLineTo c a b
path _ =
  return ()

-- | Draw some text onto the canvas.
text :: Point -> String -> Picture ()
text (x, y) str = Picture $ \ctx -> callFunction $ jsDrawText ctx str x y

textStyle :: String -> Picture () -> Picture ()
textStyle s (Picture pict) = Picture $ \ctx -> do
  callFunction $ jsPushState ctx
  callFunction $ jsTextStyle ctx s
  pict ctx
  callFunction $ jsPopState ctx
