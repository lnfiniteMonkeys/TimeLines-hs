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
























--Variables relevant to the whole piece
let 
  d = 5 -- total duration of piece/section
  reps = 16*2
  notes = fromList [1..reps]
  n = fromList [0, 3, 5, 7,   9, 7, 11, 11,   12, 9, 9, 7,   4, 4, 9, 2]
  scl = fromList [0, 2.. 16]
  note = rand . flor . (reps*) 

--"s" takes a string for the parameter to control, and a function of
--time in the form of a lambda. It then evaluates the function for
--the current global time window, saves it to a file, and reloads it on the server
    

--reloadSC













s "amp" $ \t -> sin t
