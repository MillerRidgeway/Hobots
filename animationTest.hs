module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 720
height = 480
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = animate window background drawing

drawing :: Float -> Picture
drawing f = pictures 
  [ translate f f (color ballColor $ circleSolid 10)
   ,translate f f (color paddleColor $ rectangleSolid 5 15)
  --, translate (-50) (-50) (color ballColor $ circleSolid 10)
  --, translate (-50) (-50) (color paddleColor $ rectangleSolid 5 15)
  --, translate (-200) (30) (color ballColor $ circleSolid 10)
  --, translate (-200) (30) (color paddleColor $ rectangleSolid 5 15)
  --, translate (-300) (180) (color ballColor $ circleSolid 10)
  --, translate (-300) (180) (color paddleColor $ rectangleSolid 5 15)
  , translate (-120) 0 (color boxColor $ rectangleWire 400 400)
  , translate 150 150 (color boxColor $ scale 0.25 0.25 $ text "Robot 1")
  , translate 150 50 (color boxColor $ scale 0.25 0.25 $ text "Robot 2")
  , translate 150 (-50) (color boxColor $ scale 0.25 0.25 $ text "Robot 3")
  , translate 150 (-150) (color boxColor $ scale 0.25 0.25 $ text "Robot 4")
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)
    boxColor = light green