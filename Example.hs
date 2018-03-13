module Synth where

import Context
import Util
import Data.Fixed

import TimeLines

--Sets the current working window to be the first 10 seconds of the piece

p1 = 0
p2 = 5
window p1 p2


bass = synth FM $ do 
  s "amp" $ \t -> scaleB (sin (2*pi*t * 3)) 0.01 0.4
  --  
  s "freq" $ \t -> 150 + 150*2*sin $ 2*pi*t *20
  
  --  
  s "index" $ \t -> 1 * sin $ 2*pi * t
  
  --  
  s "ratio" $ \t -> 10 + 100 * rand $ 0.0000000002*t


synth :: String -> IO Int -> IO()
