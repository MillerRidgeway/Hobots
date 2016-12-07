module Engine where

type GameState = [RobotState]  

data RobotState = RobotState {x :: Double, 
                              y :: Double, 
                              damage :: Int, 
                              speed :: Double, 
                              heading :: Double, 
                              scanInfo :: Double, 
                              id :: Int,
                              rStep ::  Stepper } -- rStep is a generic step 
                                                              -- which pushes the robot forward with a specific step when called
data Stepper = Stepper (RobotState -> (Command, Stepper))
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


setDir :: Double -> Double -> RobotState -> RobotState -- Sets direction and velocity of a given robot in the new state list
setDir h v r = r {heading = h, speed = v} -- Set velocity and heading of robot r
                    
rScan :: Double -> Double -> [RobotState] -> RobotState -> RobotState -- Degree, Variance, All Robots, Current Robot, New Robot from Current
rScan deg res robs r = let (rx, ry) = r {x,y}
                            posDeg = deg + res
                            negDeg = deg - res
                            
                            

rDrive :: RobotState -> RobotState -- All robots are driving at each step, so we simply move the state forward in terms of location

rDamage :: Double -> Double -> RobotState -> RobotState -- X coord, Y coord, robot firing, and a damaged robot
rDamage ex ey r =  let (rx, ry) = r {x, y}
                      in if rx == ex && ry == ey then r {damage = damage r - 10}
                         else r {damage = damage r}

rFire :: Double -> Double -> RobotState -> (Double, Double) -- Degree, Distance, robot, location of explosion to be added to explosions
rFire deg dist = let (rx, ry) = r {x, y}
                      fx = dist*cos(deg)
                      fy = dist*sin(deg)
                      in if sqrt ((rx-fx)^2 + (ry-fy)^2) < 700 then (fx,fy)
                         else (-300,-300)