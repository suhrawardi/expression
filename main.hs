--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Main where
import SOE

-- actionList = [putStr2 "Hello World\n",
--               writeFile "testfile.txt" "Howdy File system",
--               putStr2 "File written!\n"]

putCharList :: String -> [IO()]
putCharList = map putChar

putStr2 :: String -> IO()
putStr2 s = sequence_ (putCharList s)

pic1 = withColor Red (ellipse (150, 150) (300, 200))
pic2 = withColor Green (polyline [(100, 50), (200, 50), (200, 250), (100, 50)])
pic3 = withColor Blue (shearEllipse (10, 50) (120, 50) (80, 250))
pic4 = withColor Yellow (line (10, 150) (160, 150))

main :: IO ()
-- main = sequence_ actionList
main = runGraphics (
       do w <- openWindow "My first program" (300, 300)
          drawInWindow w pic1
          drawInWindow w pic2
          drawInWindow w pic3
          drawInWindow w pic4
          spaceClose w
       )

spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w
                              else spaceClose w
