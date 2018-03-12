module Synth where

import Context
import Util
import Data.Fixed

--Sets the current working window to be the first 10 seconds of the piece

p1 = 0
p2 = 5
window p1 p2

s "amp" $ \t -> scaleB (sin (2*pi*t * 3)) 0.01 0.4
                

  --  
s "freq" $ \t -> 150
  
  --  
s "index" $ \t -> 150 * sin $ 2*pi * t
  
  --  
s "ratio" $ \t -> 10 + 100 * rand $ 0.0000000002*t


synth :: String -> IO Int -> IO()
