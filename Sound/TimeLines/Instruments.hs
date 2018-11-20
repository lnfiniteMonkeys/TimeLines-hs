module Sound.TimeLines.Instruments where

import Prelude as Pr
import Sound.TimeLines.Types
import Sound.TimeLines.Constants
import Sound.TimeLines.Util
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

range lo hi sig = lo + f*sig
  where f = hi - lo

realTime phasor dur mult = (fast mult phasor) * (dur/mult) 


-- | Indexes into a number of unipolar random numbers using a phasor
randoms :: Signal Value -> Signal Value -> Signal Value -> Signal Value
randoms num offset sig = rand $ add offset $ flor $ num*sig

-- | Indexes into a number of bipolar random numbers using a phasor
randomsBi :: Signal Value -> Signal Value -> Signal Value -> Signal Value
randomsBi num offset sig = uniToBi $ randoms num offset sig

helperChord :: Scale -> Signal Value -> Int -> Signal Value
helperChord scale numSig voiceNum = Signal $ \t ->
  let index = floor (runSig numSig t)
      voiceIndex = Pr.mod (index + 2*voiceNum) 7
      scale' = map (flip runSig 0) scale
  in scale'!!voiceIndex


-- | Raises a (normalised) signal's minimum value
raise :: Signal Value -> Signal Value -> Signal Value
raise amt sig = amt +(1-amt)*sig

-- | Takes a scale and a list of degrees and returns a list of chords
degreesToChords :: Scale -> [Signal Value] -> ChordProg
degreesToChords scale deg = map (degree scale) deg

-- | Returns a list of ratios representing a degree of a scale
degree :: Scale -> Signal Value -> [Signal Value]
degree scale degreeNum = map semi $ chordsFromScale scale degreeNum

-- | Shortcut for indexing into a list of semitones
melody list sig = semi $ fromList list sig

chordsFromScale :: Scale -> Signal Value -> [Signal Value]
chordsFromScale scale num = map (helperChord scale num) [0..3]

-- | Takes a chord progression, a number of octaves, and a
-- | normalised phasor and arpeggiates them
arpeggio :: ChordProg -> Signal Value -> Signal Value -> Signal Value
arpeggio chords octaves sig =
  let loop = sequenceLists chords $ wrap01 $ octaves*sig
      octave = 12 * (flor $ octaves * sig)
  in  loop + octave


-- | Flattens a list of lists
flattenLists :: [[Signal a]] -> Signal Value -> [Signal a]
flattenLists listOfLists phasor = map f listOfLists
  where f list = flip fromList (wrap01 $ mul (constSig $ fromIntegral $ length listOfLists) phasor) list

-- | Takes a list of lists and a normalized phasors
-- | and sequences their elements
sequenceLists listsofLists phasor = fromList (flattenLists listsofLists phasor) phasor

--sequenceChords :: ChordProg -> Signal Value -> Chord

-- | Returns the (absolute) fractional part of a signal
fract :: (RealFrac a) => a -> a
fract x =  x - (fromIntegral $ truncate x)

-- | Floors a signal while keeping it a double
flor :: Signal Value -> Signal Value 
flor s = Signal $ \t -> fromIntegral $ floor (runSig s t)

binaryRand s = flor $ mul 2 $ rand s


-- | Indexes into a pseudo-random domain using a signal
rand :: (RealFrac a, Floating a) => Signal a -> Signal a
rand s = Signal $ \t -> fract $ (0.5 + 0.5 * (sin $ runSig s t))*1293984.31323

-- | Speeds up a signal by an amount
speed :: Signal Value -> Signal a -> Signal a
speed (Signal amt) (Signal sf) = Signal $ \t -> sf $ (amt t)*t
fast = speed

-- | Offsets a signal in time by an amount
offset :: Signal Value -> Signal a -> Signal a
offset amt s = Signal $ \t -> runSig s $ t + (runSig amt t)

-- | Slows down a signal by an amount
slow :: Signal Value -> Signal a -> Signal a
slow (Signal amt) (Signal sf) = Signal $ \t -> sf $ t/(amt t)

-- | Takes bpm, number of beats in a bar, and number of bars
-- | and returns two phasors, one for beat and one for bar,
-- | and the beat, bar, and total durations
bpmToPhasors :: Signal Value -> Signal Value -> Signal Value -> (Signal Value, Signal Value, Signal Value, Signal Value, Time)
bpmToPhasors bpm numBeats numBars =
  let beatDur = 60/bpm
      barDur = numBeats*beatDur
      totalDur = barDur*numBars
      phasorBeat = wrap01 $ t/beatDur
      phasorBar = wrap01 $ t/barDur
  in  (phasorBeat, phasorBar, beatDur, barDur, constSigToValue totalDur)


-- | Takes two signals and returns 1 if both of them are 1,
-- | otherwise 0
andGate :: (Num a, Eq a) => Signal a -> Signal a -> Signal a
andGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 && runSig v2 t == 1) then 1 else 0

-- | Takes two signals and returns 1 if either of them is 1, 
-- | otherwise returns 0
orGate :: (Num a, Eq a) => Signal a -> Signal a -> Signal a
orGate v1 v2 = Signal $ \t ->
  if (runSig v1 t == 1 || runSig v2 t == 1) then 1 else 0

-- | Takes a value in semitones and returns the ratio by which to
-- | multiply a fundamental to get that interval
semi :: (Num a, Floating a, Eq a) => Signal a -> Signal a
semi s = 2**(s/12)

-- | Takes a list of semitones and returns a list of ratios
semis ss = map semi ss


-- | Indexes into a list of Signals using a normalized signal
-- | (i.e. between 0 and 1)
fromList :: (RealFrac b) => [Signal a] -> Signal b -> Signal a
fromList vs phasor = Signal $ \t ->
  let ln = fromIntegral $ length vs
      phVal = clamp 0.0 0.99999999 (runSig phasor t)
      index = floor $ phVal*ln
  in  runSig (vs!!index) t



-- | Clamps between 0 and 1
saturate = clamp 0 1

boolToNum :: (Num a) => Bool -> a
boolToNum b
  | b == True = 1
  | b == False = 0

-- | Positive = 1, negative = 0
_sign :: (Num a, Ord a) => a -> a
_sign v = boolToNum (v <= 0)
-- Bipolar (considering 0)

-- | Positive = 1, 0 = 0, negative = -1
_signB :: (Num a, Ord a) => a -> a
_signB v
  | v == 0 = 0
  | v < 0 = -1
  | v > 0 = 1

-- | Returns 1 for positive signals and 0 for negative
sign :: (Ord a, Num a, Functor f) => f a -> f a
sign s = fmap _sign s

-- | Returns 1 for positive signals, 0 for 0, -1 for negative
signB :: (Ord a, Num a, Functor f) => f a -> f a
signB s = fmap _signB s

-- | Normalized square wave with period of 2*pi
sqr :: Signal Value -> Signal Value
sqr = sign . sin

-- Convenience functions for use with $
--add :: (Num a) => Signal a -> Signal a -> Signal a
add = liftA2 (+)
mul = liftA2 lazyMul


--Ken Perlin, "Texturing and Modeling: A Procedural Approach"
-- | Smoothstep
smoothstep :: Signal Value -> Signal Value -> Signal Value -> Signal Value
smoothstep s e t = x*x*x*(x*(x*6-15) + 10)
  where x = fmap (clamp 0 1) t

-- | Clamps a value
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

-- | Convert a bipolar wave to unipolar
biToUni v = 0.5+0.5*v

-- | Convert a unipolar wave to bipolar
uniToBi v = 1 - 2*v

-- | Normalized sine wave
sin01 :: (Fractional a, Floating a) => a -> a 
sin01 = biToUni . sin

--s1 % s2 = fmap mod'
moduloSig :: (Real a, Applicative f) => f a -> f a -> f a
moduloSig s1 s2 = liftA2 (mod') s1 s2
(%) = moduloSig

-- | Modulo 1
wrap01 s = s % 1
--mod1 = wrap01

-- | Linear interpolation using a signal
lerp :: (Num a) => Signal a -> Signal a -> Signal a -> Signal a
lerp s e phsr = Signal $ \t ->
  let v1 = runSig s t
      v2 = runSig e t
      mix = runSig phsr t
  in  (v1*(1-mix)) + (v2*mix)

-- convenience shortcuts
lerp01 = lerp 0 1
lerp10 = lerp 1 0

fromTo = lerp

-- pow x $ a + b = (a+b)**x
-- | Applies an exponent to a signal
pow :: (Floating a, Eq a) => Signal a -> Signal a -> Signal a
pow = flip (**)

--step p t = if (t < p) then 0 else 1
-- Takes a point, time, and a value, returning
-- an identity number before point and the value after
{-
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



-}

-- need to test
env atk crv1 rel crv2 t =
  let phase1 = mul (switchT 0 atk t) $ (t/atk)**crv1
      phase2 = mul (switchT atk (atk+rel) t) $ (1 - (t-atk)/rel)**crv2
  in  phase1 + phase2

-}


--switch s e sig = 

divd x = mul (1/x)


envAD atk rel crv1 crv2 phasor = Signal $ \t ->
  let a = runSig atk t
      r = runSig rel t
      c1 = runSig crv1 t
      c2 = runSig crv2 t
      t' = runSig phasor t
      v
        | t' > a + r = 0
        | t' < a = (t'/a)**c1
        | otherwise = (1 - (t'-a)/r)**c2
  in  v

switch0 s e phasor sig = Signal $ \t ->
  let t' = runSig phasor t
  in  if (t' < s || t' > e) then 0 else runSig sig t

switch1 s e phasor sig = Signal $ \t ->
  let t' = runSig phasor t
  in  if (t' < s || t' > e) then 1 else runSig sig t


{-
envR :: Signal Value -> Signal Value -> Signal Value -> Signal Value -> Signal Value -> Signal Value
envR atk rel crv1 crv2 phasor = 
  let phase1 = mul (switchT 0 atk phasor) $ pow crv1 (phasor/atk)
      phase2 = mul (switchT atk (atk+rel) phasor) $ (1 - (pow crv2 (phasor-atk)/rel))
  in  saturate $ phase1 + phase2

-}


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
