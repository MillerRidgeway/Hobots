module Engine where

--data degree :: Int -- 0-360 
--data resolution :: Int -- +/- 10 degrees
--data speed :: Int -- 0-100
type GameState  = [(RobotState, RobotState -> Command)]

data RobotState = RobotState {x :: Int, y :: Int, damage :: Int, speed :: Int, heading :: Int, scanInfo :: Int}
data MaxRobots = [RobotState]

data Command = Scan Int Int | Fire Int Int | Move Int Int 
    deriving Show 

commandStep :: RobotState -> Command
commandStep rState rCommand = 


checkStatus :: RobotState -> Bool
checkStatus r = if r {damage} /= 0 then true

rScan :: Int -> Int -> Int
rScan d r = if r > 10 then r = 10 else r = r
            if d > 359 then d = 0 else d = d
            if d < 0 then d = -d else d = d
            if fold checkStatus MaxRobots then 


main :: IO ()
main = print $ scan 0 8