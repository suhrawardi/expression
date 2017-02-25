--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Sierpinski (draw) where
import SOE

pic1 = withColor Red (ellipse (150, 150) (300, 200))
pic2 = withColor Green (polyline [(100, 50), (200, 50), (200, 250), (100, 50)])
pic3 = withColor Blue (shearEllipse (10, 50) (120, 50) (80, 250))
pic4 = withColor Yellow (line (10, 150) (160, 150))

fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size = drawInWindow w (withColor White
                       (polygon[(x, y), (x + size, y), (x, y - size), (x, y)]))

minSize :: Int
minSize = 4

sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size = if size <= minSize
                           then fillTri w x y size
                           else let size2 = size `div` 2
                             in do sierpinskiTri w x y size2
                                   sierpinskiTri w x (y - size2) size2
                                   sierpinskiTri w (x + size2) y size2

draw :: IO ()
draw = runGraphics (
       do w <- openWindow "Sirpienski" (800, 800)
          drawInWindow w pic1
          drawInWindow w pic2
          drawInWindow w pic3
          drawInWindow w pic4
          sierpinskiTri w 50 750 700
          spaceClose w
       )


spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w
