import Context
import Util
import Data.Fixed

import TimeLines


p1 = 0
p2 = 5
window 0.0 4.0

step t s = if (t < s) then 0 else 1

--let bpm = 120
    
let d = 4 -- duration in seconds
    n = 2*4 -- number of notes (3 per beat, 4 beats per loop)
    d' = d/n -- duration of each note
    
--looks a bit complicated, need to find way to avoid parens and organize visually
"amp" <>< \t -> (0.12 + (0.12 * (sin (t*2*pi*5)))) + (0.3 * (env (wrap01 (t*n)) 0.01 (d') 1.0 0.5))
  --  
"freq" <>< \t -> (150 + 100*(step t 3)) + (100 * (0.5 + 0.5 * (sin (500 *2*pi*t))))
  --  
"index" <>< \t -> 50 * (rand (t*0.00000001))
  --  
"ratio" <>< \t -> 1 + 50 * (rand (wrap01 (t*0.002)))


