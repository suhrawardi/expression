module Picture (Picture (Region, Over, EmptyPic),
                Color (Black, Blue, Green, Cyan, Red, Magenta, Yellow, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPic, draw, spaceClose, makeDrawing,
                module Region
               ) where

import Draw hiding (makeDrawing)
import Region
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
  deriving Show

data NewRegion = Rect Side Side

type Vector = (Float, Float)

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

drawRegionInWindow :: Window -> SOE.Color -> Region -> IO ()
drawRegionInWindow w c r
  = drawInWindow w
      (withColor c (drawRegion (regionToGRegion r)))

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r)   = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2; drawPic w p1
drawPic w EmptyPic       = return ()

regionToGRegion :: Region -> G.Region
regionToGRegion = regToGReg (0, 0) (1, 1)

regToNReg :: Region -> NewRegion
regToNReg = rToNR (1, 1)

rToNR :: (Float, Float) -> Region -> NewRegion
rToNR (x1, y1) (Shape (Rectangle sx sy))
             = Rect (x1 * sx) (y1 * sy)
rToNR (x1, y1) (Scale (x2, y2) r)
             = rToNR (x1 * x2, y1 * y2) r

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s)
  = shapeToGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r)
  = regToGReg loc (sx * u, sy * v) r
regToGReg (lx, ly) (sx, sy) (Translate (u, v) r)
  = regToGReg (lx + u * sx, ly + v * sy) (sx, sy) r
regToGReg loc sca Empty
  = createRectangle (0, 0) (0, 0)
regToGReg loc sca (r1 `Union` r2)
  = primGReg loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2)
  = primGReg loc sca r1 r2 andRegion
regToGReg loc sca (Complement r)
  = primGReg loc sca winRect r diffRegion

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

primGReg loc sca r1 r2 op
  = let gr1 = regToGReg loc sca r1
        gr2 = regToGReg loc sca r2
    in op gr1 gr2

shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx, ly) (sx, sy) (Rectangle s1 s2)
  = createRectangle (trans (-s1/2, -s2/2)) (trans (s1/2, s2/2))
shapeToGRegion (lx, ly) (sx, sy) (Ellipse r1 r2)
  = createEllipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGRegion (lx, ly) (sx, sy) (Polygon vs)
  = createPolygon (map trans vs)
shapeToGRegion (lx, ly) (sx, sy) (RtTriangle s1 s2)
  = createPolygon (map trans [(0, 0), (s1, 0), (0, s2)])

draw :: String -> IO ()
draw s
  = runGraphics $
    do w <- openWindow s (xWin, yWin)
       drawPic w pic1
       drawPic w pic2
       drawPic w pic3
       spaceClose w

xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5, 2.5), (-3.0, 0), (-1.7, -1.0),
                     (-1.1, 0.2), (-1.5, 2.0)])

reg1 = r3 `xUnion` (r2 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in Scale (2, 2) circle
          `Union` Translate (1, 0) square
          `Union` Translate (-1, 0) square
pic2 = Region Yellow (Translate (0, -1) reg2)

pic3 = pic2 `Over` pic1

makeDrawing :: IO ()
makeDrawing = draw "something"
