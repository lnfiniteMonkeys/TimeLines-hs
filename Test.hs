import Lib
import Signal
import Data.Fixed

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


main = do
  let d = 10
      reps = 16
      notes = fromList [1..16]
      n = fromList [0, 0, 3, 7,  8, 8, 11, 7,  3, 3, 2, 8,  7, 7, 3, 2]
  --    
  s "amp" $ \t -> let t' = wrap01 (t*reps)
                      f = t' * (d/reps)
                      atk = lerp 0.01 0.08 t
                      rel = (d/reps)*0.8
    in env f atk rel 2 2
  --  
  s "freq" $ \t -> let fund = 200
    in fund * semi $ (valueAt n t)
  --  
  s "index" $ \t -> let
    in (valueAt notes t)
  --  
  s "ratio" $ \t -> let
    in scaleB (cos $ t*d*16) 10 40
  return ()
