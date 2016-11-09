module Engine where 

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (500, 500) (10, 10)

background :: Color
background = white

--Drawing of any given robot, takes two colors 
robotDrawing :: Color -> Color -> Picture 
robotDrawing c1 c2 = pictures
  [ color ballColor $ circleSolid 10
  , color innerColor $ rectangleSolid 6 10
  ]
  where
    ballColor = c1 
    innerColor = c2

main :: IO ()
main = animate window background (robotDrawing red blue)