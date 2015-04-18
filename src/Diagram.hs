module Diagram (
    getDiagram, transformDiagram
) where

import Types
import Data.List

cube = Diagram {
    points = [ Point 1 (-100) 100    (-100) "blue"   "A"
             , Point 2 100    100    (-100) "green"  "B"
             , Point 3 100    (-100) (-100) "black"  "C"
             , Point 4 (-100) (-100) (-100) "brown"  "D"
             , Point 5 (-100) 100    100    "red"    "E"
             , Point 6 100    100    100    "red"    "F"
             , Point 7 100    (-100) 100    "red"    "G"
             , Point 8 (-100) (-100) 100    "red"    "H"
             ]
  , edges  = [ Edge 1 2 "red"    "f"
             , Edge 2 3 "blue"   "g"
             , Edge 3 4 "yellow" "h"
             , Edge 4 1 "black"  "k"
             , Edge 5 6 "red"    "l"
             , Edge 6 7 "blue"   "m"
             , Edge 7 8 "yellow" "n"
             , Edge 8 5 "black"  "o"
             , Edge 1 5 "magenta" "p"
             , Edge 2 6 "magenta" "q"
             , Edge 3 7 "magenta" "r"
             , Edge 4 8 "magenta" "s"
             ]
}

natural = Diagram {
    points = [ Point 0 (-100) 100    0      "black"    "a"
    
             , Point 1 150    100    150    "blue"     "F a"
             , Point 2 150    100    0      "magenta"  "G a"
             , Point 3 150    100    (-150) "red"      "H a"

             , Point 4 (-100) (-100) 0      "black"    "b"
             
             , Point 5 150    (-100) 150    "blue"     "F b"
             , Point 6 150    (-100) 0      "magenta"  "G b"
             , Point 7 150    (-100) (-150) "red"      "H b"
             ]
  , edges  = [ Edge 0 1 "blue"    "F"
             , Edge 0 2 "magenta" "G"
             , Edge 0 3 "red"     "H"

             , Edge 4 5 "blue"    "F"
             , Edge 4 6 "magenta" "G"
             , Edge 4 7 "red"     "H"

             , Edge 0 4 "black"   "f"
             , Edge 1 5 "blue"    "F f"
             , Edge 2 6 "magenta" "G f"
             , Edge 3 7 "red"     "H f"

             , Edge 1 2 "green"   "αa"
             , Edge 2 3 "green"   "βa"
             , Edge 5 6 "green"   "αb"
             , Edge 6 7 "green"   "βb"
             ]
}

eyeZ :: Double
eyeZ = 500

getDiagram :: Diagram
getDiagram = transformDiagram (Vector2 (-300) 160) natural

transformDiagram :: Vector2 -> Diagram -> Diagram
transformDiagram d diag = zSort $ diag { points = map (transformPoint  a b) (points diag) }
  where -- convert delta to two angles (somewhat arbitrarily)
      a = atan2 (-(vx d)) eyeZ
      b = atan2 (-(vy d)) eyeZ

transformPoint :: Double -> Double -> Point -> Point
transformPoint a b pt = pt { x = x', y = y', z = z' }
  where
      (x', y', z') = rotatePt a b (x pt, y pt, z pt)

rotatePt :: Double -> Double -> (Double, Double, Double) -> (Double, Double, Double)
rotatePt a b = rotateX a . rotateY b

rotateX :: Double -> (Double, Double, Double) -> (Double, Double, Double)
rotateX a (x, y, z) = (co * x + si * z, y, co * z - si * x)
   where co = cos a
         si = sin a

rotateY :: Double -> (Double, Double, Double) -> (Double, Double, Double)
rotateY a (x, y, z) = (x, co * y - si * z, co * z + si * y)
   where co = cos a
         si = sin a

zSort :: Diagram -> Diagram
zSort diag = diag { points = sortBy cmpPoints (points diag) }
  where
    cmpPoints :: Point -> Point -> Ordering
    cmpPoints p1 p2 = compare (negate $ z p1) (negate $ z p2)