import Context

--Below is what currently works, using intero for syntax highlighting and commint with the interpreter



window 0 7


synth "perc" "fm" $ do
  "amp" <>< \t -> scaleB 0 0.2 $ sin $ 2*pi*t * (lerp 30 80 $ t/10)                   
  "freq" <>< \t -> scaleB (cos (2*pi*t *400)) 300 550 + switch 2 20 * sin t
  "ratio" <>< \t -> 10
  "index" <>< \t -> 50
