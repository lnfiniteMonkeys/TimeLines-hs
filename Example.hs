module Synth where

import Context
import Util
import Data.Fixed

--Sets the current working window to be the first 10 seconds of the piece
window 0 10





--Variables relevant to the whole piece
let 
  d = 5 -- total duration of piece/section
  reps = 16*2
  notes = fromList [1..reps]
  n = fromList [0, 3, 5, 7,   9, 7, 11, 11,   12, 9, 9, 7,   4, 4, 9, 2] -- 
  scl = fromList [0, 2.. 16]
  note = rand . flor . (reps*) 

--"s" takes a string for the parameter to control, and a function of
--time in the form of a lambda. It then evaluates the function for
--the current global time window, saves it to a file, and reloads it on the server

synth :: Synth -> IO Int -> IO()
synth = undefined

synth FM $ do
  s "amp" $ \t ->
    --local variables relevant only to the current parameter
    let t' = wrap01 (t*reps)
        f = t' * (d/reps)
        
        atk = scale (rand . flor $ t * reps) 0.002 0.01
        rel = (d/reps)*0.90
    in 0.2 * env f atk rel (lerp 1 3 t) (lerp 4 1 t)
  
  --  
  s "freq" $ \t ->
    let fund = 200
        f = 3
        lfo = sin $ (t*d)*2*pi*f
    in 5*lfo + (fund * semi $ valueAt n t)
  
  --  
  s "index" $ \t -> let t' = flor $ t*4
                        f = 200
  
    in f * rand t' -- stepping through the pseudo-random function 4 times
  
  --  
  s "ratio" $ \t -> scale (rand (flor (t * reps))) 20 80
    

--reloadSC
