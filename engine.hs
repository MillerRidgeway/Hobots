module Engine where
import Graphics.Gloss


type GameState = [RobotState]  

data RobotState = RobotState {rsx :: Float, 
                              rsy :: Float, 
                              damage :: Int, 
                              speed :: Float, 
                              heading :: Float, 
                              scanInfo :: Float, 
                              rsid :: Int,
                              rStep ::  Stepper } -- rStep is a generic step 
                                                              -- which pushes the robot forward with a specific step when called

testRobot = RobotState {rsx = -200, rsy = -40, damage = 100, speed = 1, heading = pi/4, scanInfo = 0, rsid = 0, rStep = circleDrive 0 10 0}
testRobot2 = RobotState {rsx = -100, rsy = 0, damage = 100, speed = 1, heading = pi/4, scanInfo = 0, rsid = 0, rStep = squareDrive 0 50 0}

drive = Stepper (\r -> ((Drive 0 1),drive))

steps = map (\r -> (rsx r, rsy r)) $ map head $ take 20 $ iterate gameStep [testRobot]


squareDrive :: Int -> Int -> Float -> Stepper
squareDrive time len angle = Stepper (\r -> (Drive angle 3,if time == len then squareDrive 0 len (angle + (pi/2))
                                                             else squareDrive (time+1) len angle))

circleDrive :: Float -> Float -> Float -> Stepper
circleDrive time circumf angle = Stepper (\r -> (Drive angle 3, if time == circumf then circleDrive 0 circumf (angle + (0.45))
                                                                  else circleDrive (time+1) circumf angle))
instance Show RobotState
         where show r = "ID: " ++ show (rsid r) ++
                        " X: "   ++ show (rsx r) ++
                        " Y: "   ++ show (rsy r) ++
                        " Speed: "   ++ show (speed r) ++
                        " Heading: "   ++ show (heading r) ++
                        " Damage: " ++ show (damage r)
                                                  
data Stepper = Stepper (RobotState -> (Command, Stepper))
data Command = Scan Float Float | Fire Float Float | Drive Float Float 
    deriving Show 

gameStep :: [RobotState] -> [RobotState]
gameStep robots = let (explosions, robots') = foldl (bigStep robots) ([],[]) robots in --Define an inital list of robots and explosions
                       foldl (\robots'' (x,y) -> map (rDamage x y) robots'') robots' explosions --Damage robots with an explosion and produce 
                                                                                                --new lists of robots and explosions 

bigStep :: [RobotState] -> ([(Float, Float)],[RobotState]) -> RobotState -> ([(Float, Float)],[RobotState]) -- Creates an updated robot state 
                                                                                                                -- and explosion list by pushing a modified robot
                                                                                                                -- the robot is modified by rStep, which is 
                                                                                                                -- determined by the function as well
bigStep allRobots (explosions,newRobots) robot =
    case com of 
        Scan degree res -> (explosions, (rDrive $ rScan degree res allRobots r') : newRobots ) -- Scan case, drive (you always drive), scan, and update the lists
        Fire degree dist -> ((rFire degree dist r') : explosions, (rDrive r') : newRobots )
        Drive dir vel -> (explosions, (rDrive (setDir dir vel r')) : newRobots )
    where Stepper f = rStep robot 
          (com, step') = f robot
          r' = robot {rStep = step'} 
          
 --Scanning                  
rScan :: Float -> Float -> [RobotState] -> RobotState -> RobotState -- Degree, Variance, All Robots, Current Robot, New Robot from Current
rScan deg res robs r = let (rx, ry) = (rsx r,rsy r)
                           posDeg = deg + res
                           negDeg = deg - res
                           v1x = (2000*cos(posDeg))
                           v1y = (2000*sin(negDeg))
                           v2x = (2000*cos(posDeg))
                           v2y = (2000*sin(negDeg))
                           middleX = (v1x + v2x) /2
                           middleY = (v1y + v2y) /2
                           inRange =  filter (\r' -> scanHelp middleX middleY r' res) robs
                           dists = map (\r' -> distance (rx,ry) (rsx r',rsy r')) inRange
                           sc = minimum (dists ++ [10000])
                       in r { scanInfo = sc }
                             
degrees :: Float -> Float
degrees d = d*(180/pi)

distance :: (Float, Float) -> (Float,Float) -> Float                             
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2      
      
vdot (x1,y1) (x2, y2) = x1*x2 + y1*y2
 
scanHelp :: Float -> Float -> RobotState -> Float -> Bool
scanHelp vx vy r res = let (ox, oy) = (rsx r,rsy r)
                           numerator = vdot (vx,vy) (ox,oy)
                           denom = sqrt (ox^2+oy^2) * sqrt (vx^2 + vy^2)
                           rad = acos (numerator/denom)
                           theta = degrees rad
                       in theta <= res/2 
                                              
--Driving
rDrive :: RobotState -> RobotState -- All robots are driving at each step, so we simply move the state forward in terms of location
rDrive r = r {rsx = (rsx r) + (cos(heading r) * (speed r)), rsy = (rsy r) + (sin(heading r) * (speed r)) }

setDir :: Float -> Float -> RobotState -> RobotState -- Sets direction and velocity of a given robot in the new state list
setDir h v r = r {heading = h, speed = v} -- Set velocity and heading of robot r

rDamage :: Float -> Float -> RobotState -> RobotState -- X coord, Y coord, robot firing, and a damaged robot
rDamage ex ey r =  let (rx, ry) = (rsx r, rsy r)
                      in if rx == ex && ry == ey then r {damage = damage r - 10}
                         else r {damage = damage r}

rFire :: Float -> Float -> RobotState -> (Float, Float) -- Degree, Distance, robot, location of explosion to be added to explosions
rFire deg dist r = let   (rx, ry) = (rsx r, rsy r)
                         fx = dist*cos(deg)
                         fy = dist*sin(deg)
                        in if sqrt ((rx-fx)^2 + (ry-fy)^2) < 700 then (rx + fx,ry + fy)
                             else (-300,-300)
                             
--Display 
width, height, offset :: Int
width = 720
height = 480
offset = 100

window :: Display
window = InWindow "Hobots" (width, height) (offset, offset)

background :: Color
background = black

demo :: IO ()
demo = play window background 30 [testRobot,testRobot2] (\s -> Pictures $ map stateToPic s ) (\e s -> s) (\t s -> gameStep s)

stateToPic :: RobotState -> Picture
stateToPic r = let (rx, ry) = (rsx r, rsy r)
                   ballColor = dark red
                   paddleColor = light (light blue)
                   boxColor = light green
                  in pictures [
                               translate rx ry (color ballColor $ circleSolid 10)
                               ,translate rx ry (color paddleColor $ rectangleSolid 5 15)
                               ,translate (-120) 0 (color boxColor $ rectangleWire 400 400)
                               ,translate 150 150 (color boxColor $ scale 0.25 0.25 $ text "Robot 1")
                               ,translate 150 0 (color boxColor $ scale 0.25 0.25 $ text "Robot 2")
                              ]