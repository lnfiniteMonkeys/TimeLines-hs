module Sound.TimeLines.Instruments where

import Sound.TimeLines.Types
import Data.Fixed
import Control.Applicative


slow :: Signal Value -> Signal a -> Signal a
slow (Signal amt) (Signal sf) = Signal $ \t -> sf $ (amt t)*t



--Scales
mixolydian = [0, 2, 4, 5, 7, 9, 10]
phrygian = [0, 1, 3, 5, 7, 8, 10]
dorian = [0, 2, 3, 5, 7, 9, 10]
harmMinor = [0, 2, 3, 5, 7, 8, 11]

andGate :: Signal Value -> Signal Value -> Signal Value
andGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 && runSig v2 t == 1) then 1 else 0

orGate :: Signal Value -> Signal Value -> Signal Value
orGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 || runSig v2 t == 1) then 1 else 0


semi s = 2**(s/12)
semis ss = map semi ss

--semiList :: [Value] -> Time -> Value
--semiList ss = semi $ fromList

eps = 0.00000001

-- time between 0 and 1
fromList :: (RealFrac b) => [Signal a] -> Signal b -> Signal a
fromList vs phasor = Signal $ \t ->
  let ln = fromIntegral $ length vs
      phVal = clamp 0.0 0.99999999 (runSig phasor t)
      index = floor $ phVal*ln
  in  runSig (vs!!index) t

--quant :: [Value] -> Value -> Value
--quant vls v = 

saturate = clamp 0 1

boolToNum :: (Num a) => Bool -> a
boolToNum b
  | b == True = 1
  | b == False = 0

_sign :: (Num a, Ord a) => a -> a
_sign v = boolToNum (v <= 0)
-- Bipolar (considering 0)

_signB :: (Num a, Ord a) => a -> a
_signB v
  | v == 0 = 0
  | v < 0 = -1
  | v > 0 = 1

sign :: (Ord a, Num a, Functor f) => f a -> f a
sign s = fmap _sign s

signB :: (Ord a, Num a, Functor f) => f a -> f a
signB s = fmap _signB s

sqr :: Signal Value -> Signal Value
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

sin01 :: (Fractional a, Floating a) => a -> a 
sin01 = biToUni . sin

--s1 % s2 = fmap mod'
--moduloSig :: (Real a, Functor f) => f a -> f a -> f a
moduloSig s1 s2 = liftA2 (mod') s1 s2
s1 % s2 = moduloSig s1 s2

wrap01 s = s % 1
--mod1 = wrap01

lerp :: (Num a) => Signal a -> Signal a -> Signal a -> Signal a
lerp s e phsr = Signal $ \t ->
  let v1 = runSig s t
      v2 = runSig e t
      mix = runSig phsr t
  in  (v1*(1-mix)) + (v2*mix)
  
fromTo = lerp

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
env :: Value -> Value -> Value -> Value -> Value -> Value
env atk rel c1 c2 t
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2

{-
fract v = v - (fromIntegral $ floor v) 
rand t = fract $ 123456.9 * sin t
flor v = v - fract v
-}
