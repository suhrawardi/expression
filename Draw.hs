module Draw (inchToPixel, pixelToInch, intToFloat, makeDrawing,
             xWin, yWin, trans, shapeToGraphic, spaceClose
            ) where

import Shape
import SOE

xWin, yWin :: Int
xWin = 600
yWin = 500

xWin2, yWin2 :: Int
xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

type ColoredShapes = [(Color, Shape)]
shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]

inchToPixel :: Float -> Int
inchToPixel x = round (100 * x)

pixelToInch :: Int -> Float
pixelToInch n = intToFloat n/100

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

trans :: Vertex -> Point
trans (x, y) = (xWin2 + inchToPixel x, yWin2 - inchToPixel y)

transList :: [Vertex] -> [Point]
transList = map trans

shapeToGraphic :: Shape -> Graphic
shapeToGraphic (Rectangle s1 s2)
    = let s12 = s1 / 2
          s22 = s2 / 2
      in polygon (transList
           [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)])
shapeToGraphic (Ellipse r1 r2)
    = ellipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGraphic (RtTriangle s1 s2)
    = polygon (transList [(0, 0), (s1, 0), (0, s2)])
shapeToGraphic (Polygon vts)
    = polygon (transList vts)

sh1, sh2, sh3, sh4 :: Shape
sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w
    = mapM_ aux
      where aux (c, s) = drawInWindow w (withColor c (shapeToGraphic s))

coloredCircles =
  let colors = [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
      circles = map circle [2.4, 2.1..0.3]
  in zip colors circles

makeDrawing :: IO ()
makeDrawing = runGraphics (
                do w <- openWindow "Drawing" (xWin, yWin)
                   drawShapes w coloredCircles
                   spaceClose w
              )

spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w
