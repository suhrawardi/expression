--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
module Puts (write) where

actionList = [putStr2 "Sierpinski\n",
              writeFile "testfile.txt" "Howdy File system",
              putStr2 "File written!\n"]

putCharList :: String -> [IO()]
putCharList = map putChar

putStr2 :: String -> IO()
putStr2 s = sequence_ (putCharList s)

write :: IO ()
write = sequence_ actionList
