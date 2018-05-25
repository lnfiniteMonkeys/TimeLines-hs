import Context




reset

window 0 10



do
  let gDur = 120
  reset
  window 0 gDur







do
  let 
  synth "bob_fm" $ do
    "amp" <>< \t -> let 
      in 0.0
    "freq" <>< \t -> let s = semi $ fromList [0, 0, 0, 0, 0, 4] $ wrap01 $ t/10
      in 100*s
    "ratio" <>< \t -> let rt = mul 0.5 $ fromList [2, 3] $ wrap01 $ t/10
      in 9
    "index" <>< \t -> let rt = mul 2 $ fromList [2, 2, 3, 2, 2, 6] $ wrap01 $ t/5
      in 3000 * (lerp 1 0 $ pow 0.1 $ wrap01 $ t*8)
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






--the window function modifies a global IORef that's used to determine
--the time interval over which to evaluate all timelines
--(ideally it should keep track of which synths/parameters are being controlled
--and update them when the window changes, so that it acts more like a global transport)
--will also be bound to keyboard shortcut

window 0 10



