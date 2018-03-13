module Synth where

import Context
import Util
import Data.Fixed

import TimeLines

--Sets the current working window to be the first 10 seconds of the piece

p1 = 0
p2 = 5
--window p1 p2



--let bpm = 120
    

bass = do
  "amp" <>< \t ->
    10*t
  --  
  "freq" <>< \t ->
    150*t
  --  
  "index" <>< \t ->
    200
  --  
  "ratio" <>< \t ->
    150

