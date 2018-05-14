module Main where

import Lib
import Graphics.Gloss

main :: IO ()
main = simulate (InWindow "Nice Window" (200, 200) (10, 10)) 
       white 30 
       (0,0) 
       (\(theta,dtheta) -> Line [(0,0), (40 * cos theta, 40 * sin theta)]) 
       (\_ dt (theta, dtheta) -> (theta + dt * dtheta,dtheta - dt * (cos theta)))


-- animate (InWindow "Nice Window" (200, 200) (10, 10)) white (\t -> Line [(0,0), (40 * cos t, 40 * sin t)])

--display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)