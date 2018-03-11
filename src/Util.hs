module Util where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

import Data.Fixed
import Signal

--Range list with number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)

--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


-- Linear interpolation
lerp a b f = a*(1-f) + b * f


(%) = mod'


--Takes Time and a start and end point and goes from 0 to 1 while Time
--travels between those two points, returning 0 otherwise.
zto1 :: (Floating a, Ord a) => a -> a -> a -> a
zto1 t s e
  | t < s = 0
  | t > e = 0
  | otherwise = (t - s) / (e - s)

--Returns 1 for every moment in Time that is within lo and hi, 0 otherwise
switch :: (Num a, Ord a) => a -> a -> a -> a
switch t lo hi = if lo <= t && t <= hi
                 then 1
                 else 0


fromList :: [a] -> Signal a
fromList xs = Signal $ \t -> let len = fromIntegral $ length xs
                                 index = floor $ t*len
                             in  xs!!index 


semi s = 2**(s/12)


fract t = snd $ properFraction t
rand t = fract $ 987654321 * sin $ t*10000
scale v lo hi = lo + v*hi
scaleB v lo hi = scale v' lo hi --for bipolar functions
  where v' = 1 + 0.5*v
  
flor :: RealFrac a => a -> a
flor s = fromIntegral $ floor s

wrap01 :: RealFrac a => a -> a
wrap01 s = mod' s 1


env t atk rel c1 c2
  | t > atk + rel = 0
  | t < atk = (t/atk)**c1
  | otherwise = (1 - (t-atk)/rel)**c2

