module Sound.TimeLines.Instruments where

import Prelude as Pr
import Sound.TimeLines.Types
import Sound.TimeLines.Constants
import Data.Fixed
import Control.Applicative

--TODO: test
{-
arpeggio :: ChordProg -> Signal Value -> Signal Value -> Signal Value
arpeggio chords octaves sig =
  let firstOctaveLoop = fromLists chords $ wrap01 $ octaves * sig
      octaves = 12 * (flor $ octaves * sig)
  in  firstOctaveLoop + octaves
  --  firstOctaveloop * (fromList [0..octaves] sig)
-}

{-
scaleToChords :: Scale -> ChordProg
scaleToChords scale = map f [0..l]
  where l = length scale
        f = \x -> [scale!!x, scale!!((x+2)%l), scale!!((x+4)%l)]
-}


randoms :: Signal Value -> Signal Value -> Signal Value -> Signal Value
randoms num offset sig = rand $ add offset $ flor $ num*sig

randomsBi :: Signal Value -> Signal Value -> Signal Value -> Signal Value
randomsBi num offset sig = uniToBi $ randoms num offset sig

helperChord :: Scale -> Signal Value -> Int -> Signal Value
helperChord scale numSig voiceNum = Signal $ \t ->
  let index = floor (runSig numSig t)
      voiceIndex = Pr.mod (index + 2*voiceNum) 7
      scale' = map (flip runSig 0) scale
  in scale'!!voiceIndex


raise :: Signal Value -> Signal Value -> Signal Value
raise amt sig = amt +(1-amt)*sig

degreesToChords :: Scale -> [Signal Value] -> ChordProg
degreesToChords scale deg = map (chords scale) deg

chords :: Scale -> Signal Value -> [Signal Value]
chords scale chordNum = map semi $ chordsFromScale scale chordNum

melody list sig = semi $ fromList list sig

chordsFromScale :: Scale -> Signal Value -> [Signal Value]
chordsFromScale scale num = map (helperChord scale num) [0..3]

arpeggio :: ChordProg -> Signal Value -> Signal Value -> Signal Value
arpeggio chords octaves sig =
  let loop = sequenceLists chords $ wrap01 $ octaves*sig
      octave = 12 * (flor $ octaves * sig)
  in  loop + octave

flattenLists :: [[Signal a]] -> Signal Value -> [Signal a]
flattenLists listOfLists phasor = map f listOfLists
  where f list = flip fromList (wrap01 $ mul (constSig $ fromIntegral $ length listOfLists) phasor) list

sequenceLists listsofLists phasor = fromList (flattenLists listsofLists phasor) phasor

--sequenceChords :: ChordProg -> Signal Value -> Chord

fract :: (RealFrac a) => a -> a
fract x =  x - (fromIntegral $ truncate x)

--flor :: Sig
flor :: Signal Value -> Signal Value 
flor s = Signal $ \t -> fromIntegral $ floor (runSig s t)

rand :: (RealFrac a, Floating a) => Signal a -> Signal a
rand s = Signal $ \t -> fract $ (sin $ runSig s t)*99999999

speed :: Signal Value -> Signal a -> Signal a
speed (Signal amt) (Signal sf) = Signal $ \t -> sf $ (amt t)*t
fast = speed

offset :: Signal Value -> Signal a -> Signal a
offset amt s = Signal $ \t -> runSig s $ t + (runSig amt t)

slow :: Signal Value -> Signal a -> Signal a
slow (Signal amt) (Signal sf) = Signal $ \t -> sf $ t/(amt t)

bpmToPhasors :: Signal Value -> Signal Value -> Signal Value -> (Signal Value, Signal Value, Signal Value, Signal Value, Signal Value)
bpmToPhasors bpm numBeats numBars =
  let beatDur = 60/bpm
      barDur = numBeats*beatDur
      totalDur = barDur*numBars
      phasorBeat = wrap01 $ t/beatDur
      phasorBar = wrap01 $ t/barDur
  in  (phasorBeat, phasorBar, beatDur, barDur, totalDur)




andGate :: (Num a, Eq a) => Signal a -> Signal a -> Signal a
andGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 && runSig v2 t == 1) then 1 else 0

orGate :: (Num a, Eq a) => Signal a -> Signal a -> Signal a
orGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 || runSig v2 t == 1) then 1 else 0

semi :: (Num a, Floating a, Eq a) => Signal a -> Signal a
semi s = 2**(s/12)
semis ss = map semi ss


-- time between 0 and 1
-- need to make it work with lists of lists too
fromList :: (RealFrac b) => [Signal a] -> Signal b -> Signal a
fromList vs phasor = Signal $ \t ->
  let ln = fromIntegral $ length vs
      phVal = clamp 0.0 0.99999999 (runSig phasor t)
      index = floor $ phVal*ln
  in  runSig (vs!!index) t


{-
sequenceLists :: (RealFrac b) => [[Signal a]] -> Signal b -> [Signal a]
sequenceLists ls phasor = Signal $ \t ->
  let ln = fromIntegral $ length ls
      phVal = clamp 0.0 0.99999999 (runSig phasor t)
      index = floor $ phVal*ln
  in  map (runSig ls!!index) t
-}

list :: (RealFrac b) => Signal [a] -> Signal b -> Signal a
list vs s = Signal $ \t ->
  let ln = fromIntegral $ length $ runSig vs t
      phVal = clamp 0.0 0.99999999 (runSig s t)
      index = floor $ phVal*ln
  in  (runSig vs t)!!index


--fromList :: (RealFrac b, Functor f) => [Signal a] -> Signal b -> Signal a
--fromList l phasor =  

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
add :: (Num a) => Signal a -> Signal a -> Signal a
add = liftA2 (+)
mul :: (Num a) => Signal a -> Signal a -> Signal a
mul = liftA2 (*)
--mul = (*.)


--Ken Perlin, "Texturing and Modeling: A Procedural Approach"

smoothstep :: Signal Value -> Signal Value -> Signal Value -> Signal Value
smoothstep s e t = x*x*x*(x*(x*6-15) + 10)
  where x = fmap (clamp 0 1) t


clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

biToUni v = 0.5+0.5*v

uniToBi v = 1 - 2*v

sin01 :: (Fractional a, Floating a) => a -> a 
sin01 = biToUni . sin

--s1 % s2 = fmap mod'
moduloSig :: (Real a, Applicative f) => f a -> f a -> f a
moduloSig s1 s2 = liftA2 (mod') s1 s2
(%) = moduloSig

wrap01 s = s % 1
--mod1 = wrap01

lerp :: (Num a) => Signal a -> Signal a -> Signal a -> Signal a
lerp s e phsr = Signal $ \t ->
  let v1 = runSig s t
      v2 = runSig e t
      mix = runSig phsr t
  in  (v1*(1-mix)) + (v2*mix)

lerp01 = lerp 0 1
lerp10 = lerp 1 0

fromTo = lerp

-- pow x $ a + b = (a+b)**x
pow :: (Floating a, Eq a) => Signal a -> Signal a -> Signal a
pow = flip (**)

--step p t = if (t < p) then 0 else 1
-- Takes a point, time, and a value, returning
-- an identity number before point and the value after

step p v = Signal $ \t ->
  if (t < runSig p t) then 0 else (runSig v t)

--for use with non-signal values
step' p t
  | t < p = 0
  | otherwise = 1

step1 p v = Signal $ \t ->
  if (t < runSig p t) then 1 else (runSig v t)

switch s e v = Signal $ \t ->
  if (t < runSig s t || t > runSig e t) then 0 else (runSig v t)

switchT s e tm = Signal $ \t ->
  let s' = runSig s t
      e' = runSig s t
      tm' = runSig tm t
  in  if (tm' < s' || tm' > e') then 0 else 1
  
--for use with non-signal values
switch' s e t
  | t < s || t > e = 0
  | otherwise = 1
  
switch1 s e v = Signal $ \t ->
  if (t < runSig s t || t > runSig e t) then 1 else (runSig v t)

--Simple AD envelope driven by an input t in seconds, increasing from 0
--env :: Value -> Value -> Value -> Value -> Value -> Value

{-
env atk rel c1 c2 t
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2
-}

-- need to test
env atk crv1 rel crv2 t =
  let phase1 = mul (switchT 0 atk t) $ (t/atk)**crv1
      phase2 = mul (switchT atk (atk+rel) t) $ (1 - (t-atk)/rel)**crv2
  in  phase1 + phase2


divd x = mul (1/x)

--parabola test
para x = divd 4 $ a*(x*d-b)**2 + 4
  where a = -0.3
        b = 3.7
        d = 7.3



{-
envlp :: (Floating a, Ord a) => Signal a -> Signal a -> Signal a -> Signal a -> Signal a -> Signal a
envlp atk rel c1 c2 ph = Signal $ \t ->
  let t' = runSig ph t
      atk' = runSig atk t
      rel' = runSig rel t
      c1' = runSig c1 t
      c2' = runSig c2 t
      phase1 = mul (switch' 0 atk' t') $ ((t'/atk')**c1')
      phase2 = mul (switch' atk' (atk' + rel') t') $ (1 - (t'-atk')/rel')**c2'
  in  phase1 + phase2
-}




{-
fract v = v - (fromIntegral $ floor v) 
rand t = fract $ 123456.9 * sin t
flor v = v - fract v
-}
