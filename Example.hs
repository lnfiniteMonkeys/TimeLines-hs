


{-
bass = do
  synth "fm" $ \t ->
    let somestuff = 3
        someOtherstuff = 10
        foo = 3.14
      
    "amp"   <><  sin t
               + cos t
               + 0.5 * sin $ 2*pi*t
               + onAt 3.5 15.0 t $ 100 * sin $ t*20
    "freq"  <><  150
    "ratio" <><  2.5
               + onAt 0.2 5.0 t $ sin $ t*25 + t^8*25
  
  
-}






  

--The below code works just fine, but I want to see how far I can push the syntax



p1 = 0
p2 = 5
--window 3.0 5.0

step t s = if (t < s) then 0 else 1

{-
let d = 4 -- duration in seconds
    n = 2*4 -- number of notes (3 per beat, 4 beats per loop)  
    d' = d/2 -- duration of each   




"amp" <>< \t -> lfo t + env
  --  
"freq" <>< \t -> melody (wrap01 (t*5))
  --  
"index" <>< \t -> lerp 50 105 (t/8)**1 * (20 * (rand (t*0.1)))
  --  
"ratio" <>< \t -> 1 + 50 * (rand $ wrap01 (t*0.00000002))


-}

--synth :: (Time -> IO()) -> Time -> IO()




