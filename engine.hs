module Engine where

type GameState = [RobotState]  

data RobotState = RobotState {x :: Double, 
                              y :: Double, 
                              damage :: Int, 
                              speed :: Double, 
                              heading :: Double, 
                              scanInfo :: Double, 
                              id :: Int,
                              rStep :: RobotState -> Command} -- rStep is a genaric step 
															  -- which pushes the robot forward with a specific step when called

data Command = Scan Double Double | Fire Double Double | Move Double Double 
    deriving Show 

gameStep :: [RobotState] -> [RobotState]
gameStep robots = let (explosions, robots') = foldl (bigStep robots) ([],[]) robots in --Define an inital list of robots and explosions
                       foldl (\robots'' (x,y) -> map (rDamage x y) robots'') robots' explosions --Damage robots with an explosion and produce 
				                                                                                --new lists of robots and explosions 

bigStep :: [RobotState] -> ([(Double, Double)],[RobotState]) -> RobotState -> ([(Double, Double)],[RobotState]) -- Creates an updated robot state 
                                                                                                                -- and explosion list by pushing a modified robot
																												-- the robot is modified by rStep, which is 
																												-- determined by the function as well
bigStep allRobots (explosions,newRobots) robot =
    case (rStep robot) robot of 
        Scan degree res -> (explosions, rDrive $ rScan degree res robot : newRobots ) -- Scan case, drive (you always drive), scan, and update the lists
        Fire degree dist -> (rFire degree dist robot : explosions, rDrive robot : newRobots )
        Drive dir vel -> (explosions, rDrive (setDir dir vel robot) : newRobots )

setDir :: Double -> Double -> RobotState -- Sets direction and velocity of a given robot in the new state list

rScan :: Double -> Double -> [RobotState] -> RobotState -> RobotState -- Degree, Variance, All Robots, Current Robot, New Robot from Current

rDrive :: RobotState -> RobotState -- All robots are driving at each step, so we simply move the state forward in terms of location

rDamage :: Double -> Double -> RobotState -> RobotState -- X coord, Y coord, robot firing, and a damaged robot

rFire :: Double -> Double -> RobotState -> (Double, Double) -- Degree, Distance, robot, location of explosion to be added to explosions
