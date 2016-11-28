module Engine where

--data degree :: Int -- 0-360 
--data resolution :: Int -- +/- 10 degrees
--data speed :: Int -- 0-100

data RobotState = RobotState {x :: Int, y :: Int, damage :: Int, speed :: Int, heading :: Int}
data MaxRobots = [Robot]

data Command = Scan | Fire | Move 
	deriving Show 

rScan :: Int -> Int -> Int
rScan d r = if r > 10 then 10 else r
           --if d > 359 then d = 0 else d = d
           --if d < 0 then d = -d else d = d


main :: IO ()
main = print $ scan 0 8