module Engine where

--data degree :: Int -- 0-360 
--data resolution :: Int -- +/- 10 degrees

data Robot = Robot {x :: Int, y :: Int, damage :: Int, speed :: Int, heading :: Int}
data MaxRobots = [Robot]

scan :: Int -> Int -> Int
scan d r = if r > 10 then 10 else r
           --if d > 359 then d = 0 else d = d
           --if d < 0 then d = -d else d = d


main :: IO ()
main = print $ scan 0 8