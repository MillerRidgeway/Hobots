module SampleRobot where 

spin :: Double -> Double -> RController
spin k d = do r <- scan k 
              if r == 0
                 then spin (k+d) 
                 else do fire k r
                         spin (k+d) d