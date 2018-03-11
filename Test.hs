import Context
import Util
import Data.Fixed


d = 5
reps = 16*2
notes = fromList [1..reps]
n = fromList [0, 3, 5, 7,   9, 7, 11, 11,   12, 9, 9, 7,   4, 4, 9, 2]
scl = fromList [0, 2.. 16]
note = rand . flor . (reps*) 

s "amp" $ \t ->
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
s "index" $ \t -> let t' = t*4
                      f = 200
                        --env = 
  in f * rand $ flor t'
  --  
s "ratio" $ \t -> scale (rand (flor (t * reps))) 20 80
    

reloadSC
