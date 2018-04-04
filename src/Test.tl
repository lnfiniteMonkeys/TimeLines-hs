import Context

--Below is what currently works, using intero for syntax highlighting and commint with the interpreter



fromTo s e t = 
  

window 0 5


synth "bob_fm" $ do
  "amp" <>< \t -> 0.1
  "freq" <>< \t -> 200 + 25*(sin $ 2*pi*t * 20)
  "ratio" <>< \t -> 5 + 2 * (rand $ t*0.005)
  "index" <>< \t -> 100 + 2000*(rand (t*0.1)) * (rand $ t*0.02)
  "pan" <>< \t -> -0.5


synth "fred_fm" $ do
  "amp" <>< \t -> 0.1
  "freq" <>< \t -> 100  
  "ratio" <>< \t -> 5
  "index" <>< \t -> 150
  "pan" <>< \t -> 0.5 + 0.6*(sin $ 2*pi*t * 5 + t)



synth "bob" "fm" $ do
  let n = 3
      phasor t = wrap01 $ t*n      
  "amp" <>< \t -> let a = 0.2
                      b = a + 0.1*(sin (2*pi*t*1))
                      t' = 1/3 * (wrap01 $ t * 3)
    in 0.8 * (env 0.002 0.2 t' 1 1)
  "freq" <>< \t -> let t' = wrap01 $ t*(lerp 8 15 (wrap01 t))
                       f = 200
                       f' = f + 100*sin $ 2*pi*t * lerp 2 3 (wrap01 t)
    in 100 + 0.1* f' * t'
  "ratio" <>< \t -> 2 + rand $ flor $ 4 * sin (2*pi*t*1 + 0.5*(rand $ flor $ t))
  "index" <>< \t -> let
    in 500












synth "bob" "fm" $ do
  let n = 6
      phasor t = 1/n * (wrap01 $ t*n)
  "amp" <>< \t -> let a = 0.2
                      b = a + 0.1*(sin (2*pi*t*1))
                      t' = 1/3 * (wrap01 $ t * 3)
    in 0.1
  "freq" <>< \t -> let t' = wrap01 $ t*(lerp 8 15 (wrap01 t))
                       f = 200
                       f1 = f + 1*sin $ 2*pi*t * lerp 2 3 (wrap01 t)
    in 80 + 5 * (sin $ 2*pi*t*5.5)
  "ratio" <>< \t -> 0.5 * (scaleB 0.2 1 $ rand $ flor $ t*3.5)
  "index" <>< \t -> let
    in 100




    







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
