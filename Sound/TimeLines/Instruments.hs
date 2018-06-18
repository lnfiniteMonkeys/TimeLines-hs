module Sound.TimeLines.Toolkit where

import Sound.TimeLines.Types

import Data.Fixed

default (Double)

--Scales
mixolydian = [0, 2, 4, 5, 7, 9, 10]
phrygian = [0, 1, 3, 5, 7, 8, 10]
dorian = [0, 2, 3, 5, 7, 9, 10]
harmMinor = [0, 2, 3, 5, 7, 8, 11]


andGate v1 v2
  | v1 == 1 && v2 == 1 = 1
  | otherwise = 0

orGate v1 v2
  | v1 == 1 || v2 == 1 = 1
  | otherwise = 0

semi s = 2**(s/12)
semis ss = map semi ss

--semiList :: [Value] -> Time -> Value
--semiList ss = semi $ fromList

-- time between 0 and 1
fromList :: [a] -> Time -> a
fromList vs t = vs!!index
  where ln = fromIntegral $ length vs
        index = floor $ t'*ln
        t' = clamp 0.0 0.9999999 t
        

--quant :: [Value] -> Value -> Value
--quant vls v = 

saturate = clamp 0 1

sign v
  |v <= 0 = 0
  |v > 0 = 1

-- Bipolar (considering 0)
sign' v
  |v == 0 = 0
  |v < 0 = -1
  |v > 0 = 1

  
sqr = sign . sin

-- Convenience functions for use with $
add = (+)
mul = (*)
--mul = (*.)

--Ken Perlin, "Texturing and Modeling: A Procedural Approach"
smoothstep s e t = x*x*x*(x*(x*6-15) + 10)
  where x = clamp 0 1 t

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

biToUni v = 0.5+0.5*v

uniToBi v = 1 - 2*v

sin01 = biToUni . sin

(%) = mod'                     
wrap01 v = mod' v 1
mod1 = wrap01

fromTo s e t
  | t <= 0 = s
  | t > 1 = e
  | otherwise = (e*t) + (s *(1-t))

lerp = fromTo

--Example common workflow functions

-- pow x $ a + b = (a+b)**x
pow = flip (**)

--step p t = if (t < p) then 0 else 1
-- Takes a point, time, and a value, returning
-- an identity number before point and the value after
step0 p t v = if t < p then 0 else v
step1 p t v = if t < p then 1 else v

steps0 s e t v
  | (t < s || t > e) = 0
  | otherwise = v

steps1 s e t v
  | (t < s || t > e) = 1
  | otherwise = v

  
zto1 s e t = saturate $ (t-s)/(e-s)

switch s e t
  | t < s = 0
  | t > e = 0
  | otherwise = 1


--Simple AD envelope driven by an input t in seconds, increasing from 0
env :: Value -> Value -> Value -> Value -> Time -> Value
env atk rel c1 c2 t
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2

{-
fract v = v - (fromIntegral $ floor v) 
rand t = fract $ 123456.9 * sin t
flor v = v - fract v
-}
