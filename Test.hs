import Lib
import Signal
import Data.Fixed

--import Soun.OSC

fract t = snd $ properFraction t
rand t = fract $ 987654321 * sin $ t*10000
scale v lo hi = lo + v*hi
scaleB v lo hi = scale v' lo hi
  where v' = 1 + 0.5*v
  
flor :: RealFrac a => a -> a
flor s = fromIntegral $ floor s

wrap01 :: RealFrac a => a -> a
wrap01 s = mod' s 1


env t atk rel c1 c2
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2


fromList :: [a] -> Signal a
fromList xs = Signal $ \t -> let len = fromIntegral $ length xs
                             in  xs!!(floor $ t*len) 

semi s = 2**(s/12)

















[x^2 <- [0, 1..], x > 4]


main = do
  let d = 5
      reps = 16*2
      notes = fromList [1..reps]
      n = fromList [0, 3, 5, 7,   9, 7, 11, 11,   12, 9, 9, 7,   4, 4, 9, 2]
      scl = fromList [0, 2.. 16]
      note = rand . flor . (reps*) 
  --
  s "amp" $ \t ->
    let t' = wrap01 (t*reps)
        f = t' * (d/reps)
          
        atk = scale (rand . flor $ t * reps) 0.002 0.01
        rel = (d/reps)*0.90
    in 0.2 * env f atk rel (lerp 1 3 t) (lerp 4 1 t)
  --  
  s "freq" $ \t ->
    let fund = 200
        f = 3
        lfo = sin $ (t*d)*2*pi*f
    in 5*lfo + (fund * semi $ valueAt n t)
  --  
  s "index" $ \t -> let t' = t*4
                        f = 200
                        --env = 
    in f * rand $ flor t'
  --  
  s "ratio" $ \t -> scale (rand (flor (t * reps))) 20 80
    
  reloadSC
