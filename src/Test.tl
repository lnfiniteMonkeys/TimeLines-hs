import Context



do
  let gDur = 120
  reset
  window 0 gDur
  


synth "bob_fm" $ do
  "amp" <>< \t -> let
    in 0.01
  "freq" <>< \t -> let
    in 300 * (fromList [1, 3, 4, 2, 2, 1.5, 1.5] $ wrap01 $ t*3)
       + 150*(fromList [1, 1, 1.5, 1.75, 1.5] $ wrap01 $ t*0.125)
  "ratio" <>< \t -> let
    in 10
  "index" <>< \t -> let
    in 1000 * 1
  "pan" <>< \t -> 0


synth "delia_reverb" $ do
  "predelay" <>< \t -> 0.5
  "revtime" <>< \t -> 1.8
  "lpf" <>< \t -> 1500
  "mix" <>< \t -> 0.55


synth "fred_fm" $ do
  let n = 16*4
      d = gDur/n
      e1  t = env 0.005 3.5 1 5 $ (d*) $ wrap01 $ (t/gDur)*n
  "amp" <>< \t -> 0.2 * e1 t
                  + 0.5 * (e1 $ t*0.75)
                  + 0.3 * (e1 $ t*1.25)
  "freq" <>< \t -> 100
  "ratio" <>< \t -> 18
                    + (0)
  "index" <>< \t -> 100 * (e1 t)
                    + 200*(e1 $ t*0.5)
  "pan" <>< \t -> 0



synth "lucy_fm" $ do
  let n = 16*8
      d = gDur/n
      e1  t = env 0.005 0.5 1 3 $ (d*) $ wrap01 $ (t/gDur)*n
  "amp" <>< \t -> let
    in 0.1
       + 0
  "freq" <>< \t -> let
    in 80
       + (80*0.0125)*(sin $ 2*pi*t*0.25)
  "ratio" <>< \t -> let
    in 2
       + 0.05*(sin $ 2*pi*t*5)
  "index" <>< \t -> let
    in 2000 *(e1 $ t*1.33)
       + 1000*(e1 $ t*3)
  "pan" <>< \t -> 0

























p1 = 0
p2 = 5


ad = (+)

g = sin .> (20*) .> (+20)

g 0


(.>) = flip (.)

:t g

g 3.8


:t g

g



--the window function modifies a global IORef that's used to determine
--the time interval over which to evaluate all timelines
--(ideally it should keep track of which synths/parameters are being controlled
--and update them when the window changes, so that it acts more like a global transport)
--will also be bound to keyboard shortcut




window 0 10

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
    --a coment
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
