import Context

--Below is what currently works, using intero for syntax highlighting and commint with the interpreter



window 0 7


synth "perc" "fm" $ do
  "amp" <>< \t -> scaleB 0 0.2 $ sin $ 2*pi*t * (lerp 30 80 $ t/10)                   
  "freq" <>< \t -> scaleB (cos (2*pi*t *400)) 300 550
                   + switch 2 20 * sin t
  "ratio" <>< \t -> 10
  "index" <>< \t -> 50





































p1 = 0
p2 = 5
--the window function modifies a global IORef that's used to determine
--the time interval over which to evaluate all timelines
--(ideally it should keep track of which synths/parameters are being controlled
--and update them when the window changes, so that it acts more like a global transport)
--will also be bound to keyboard shortcut
window p1 p2

--Example common workflow functions
step t s = if (t < s) then 0 else 1
--Simple AD envelope driven by an input t in seconds, increasing from 0
env t atk rel e1 e2
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2


--ideally this would be used for higher-level realtime 
let interval = p2-p1
    numNotes = 8
    noteDur = interval/numNotes
    phasor t = wrap01 $ numNotes*(t/interval)
    semitones = [0, 3, 3, 7, 5, 9, 7, 1]
    fund = 200
    ind t = fromIntegral $ (floor $ numNotes*t/interval)%(numNotes)
    note t = fund * semi $ semitones!!ind (t*2)


-- evaluates the functions over the current window (set above), writes
-- them to files, and updates the buffers and synth
"amp" <>< \t -> 0.8 * env (noteDur * phasor t) 0.005 (noteDur*0.5) 1.0 2.0
--  
"freq" <>< \t -> note t + lerp 0 1 (t/interval)**1 * 100 * (sin $ 2*pi*t *8)
--  
"index" <>< \t -> 100 + 400 * env (noteDur * phasor t) 0.0 (noteDur*0.9) 1.0 $ lerp 10.0 200.0 (t/interval)
--  
"ratio" <>< \t -> 10

--(<><) :: String -> (Time -> Value) -> IO()

{-
Problems with the above:
-no way to specify which synthdef to use (hardcoded for now),
-no way to work with/refer to multiple synths or group the parameters together
-having to use a lambda for every parameter individually means I can't have a derivation
from t that's available to all parameters of a synth (e.g. t' = wrap01 $ t*10)
-}



-- Below is an example of how I think it can be improved (random code, doesn't run)
bass = synth "fm" $ \t -> --one t for all parameters in a synth (ideally multiple synths too)
    let somestuff = 3
        someOtherstuff = 10
        t' = t + 10 * sin $ 2*pi*t *10
     
    "amp"   <><  switch p1 p2 t * sin t
               + switch p2 (p2+10) t * cos t
               + 0.5 * sin $ 2*pi*t
               + onAt 3.5 15.0 t $ 100 * sin $ t*20
--being able to stack vertically like above would be great, allows to conceptually separate parts
--that don't affect the parameters at overlapping time intervals and to avoid parens
--perhaps possible by parsing each parameter's equation and removing the newlines?
    "freq"  <><  let fund = 500
                     semitones = indexList [0, 1, 2, 3, 4, 5, 6, 7] t
                     notes = semitones + (200* sin(t*2))
                 in  fund*semitones
--the let..in approach helps organize the mess and
--keep the actual equations high level
    "ratio" <><  let fund = 200
                     mult = indexList [10, 20, 20, 35, 100, 50] t
                 in  10 + fund*mult
                   + onAt 0.2 5.0 t $ sin $ t*25 + t^8*25
                   + 0.2 * sin $ t*0.5
  
  
{-
Notes:
-ideally, the bulk of functions/composition would be happen at the top of the synth,
so as to keep the actual parameter equations as simple and high-level as possible

-"synth" should take a synthdef name, and a function of time that has multiple
parameter components (something like synth :: SynthDef -> [Signal] -> IO() ?).

- the synth name (in this case "bass") should be used to refer to the nodeID of the synth running
and added to a global list of "working synths", so that when the global Window changes it can update
all synths by sending their names (in this case "bass", which has type IO()) to the interpreter, which would re-rendr
and load all parameter buffers using the new window.

-the function that sends the OSC (either "bass" or the individual (<><)s) should be aware of the synth name
so that SC knows which nodeID to apply the amp, freq etc arguments to. Is this the writer monad?

-indentation is a mess
-}

