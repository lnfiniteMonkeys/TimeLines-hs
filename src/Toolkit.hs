module Toolkit where

import Signal


fromList :: [Value] -> Time -> Value
fromList vs t = vs!!index
  where ln = fromIntegral $ length vs
        index = floor $ t*ln

saturate = clamp 0 1

sin01 t = 0.5 + 0.5 * sin t

sign v
  |v <= 0 = 0
  |v > 0 = 1
  
sqr = sign . sin

add = (+)
mul = (*)

--Ken Perlin, "Texturing and Modeling: A Procedural Approach"
smoothstep s e t = x*x*x*(x*(x*6-15) + 10)
  where x = clamp 0 1 t

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

biToUni v = 0.5+0.5*v

uniToBi v = 1 - 2*v
