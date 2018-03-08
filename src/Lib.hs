module Lib  where

--import Control.Concurrent
import qualified Sound.File.Sndfile as SF
import System.IO as IO
import Foreign.Marshal.Array as MA
import Foreign.Ptr
import Foreign.ForeignPtr as FP
import System.Directory as D

import qualified Sound.OSC as OSC
import qualified Sound.OSC.FD as FD
import Signal
import Util

import Prelude as Pr

import Data.Fixed


default (Double, Rational)

-- A timeline (file written to disk) has a start (> 0) and end point,
-- a samplerate and a string that denotes which parameter of the synth it controls
data TLinfo = TLinfo {tlStart::Time,
                      tlEnd::Time,
                      tlSR::Int,
                      tlParam::String
                     }
              deriving (Eq, Show)

-- Duration of file to be written and played back
tlDur :: TLinfo -> Time
tlDur (TLinfo s e _ _) = e - s

--tlRelStart :: TLinfo -> Time
--tlRelStart i = 

-- The number of frames that will be written to file
tlNumFrames :: TLinfo -> Int
tlNumFrames i = Pr.floor $ tlDur i * fromIntegral (tlSR i)

-- A TimeLine is made up of a signal (defined over infinite time starting from 0)
-- and a TLinfo, which contains the info needed to write a part of the signal to a file
data TimeLine = TimeLine {tlSig::Signal Value,
                          tlInfo::TLinfo
                         }


defaultSampleRate = 700
defaultInfo :: Time -> Time -> String -> TLinfo
defaultInfo s e argName = TLinfo s e defaultSampleRate argName

--Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle i = SF.openFile filename SF.ReadWriteMode info
        where filename = tlParam i ++ ".w64"
              format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (tlNumFrames i) (tlSR i) 1 format 1 True

closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- Render the TimeLine over the duration specified by the TLinfo
getVals :: TimeLine -> [Value]
getVals (TimeLine sig info@(TLinfo s e _ _)) = map f domain
  where f = valueAt sig
        domain = fromToIn s e $ tlNumFrames info


--Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: TimeLine -> IO (Ptr Value)
getArrayPtr tl = do
  ptr <- MA.newArray $ getVals tl
  return ptr


--Takes a TimeLine, writes it to a file, and returns number of frames written
writeTL :: TimeLine -> IO Int
writeTL tl@(TimeLine sig info) = do
  let fileName = tlParam info ++ ".w64"
  --IO.openFile fileName IO.ReadWriteMode
  _ <- removeIfExists fileName -----------------better way?
  h <- openHandle info
  arrayPtr <- getArrayPtr tl
  framesWritten <- SF.hPutBuf h arrayPtr $ tlNumFrames info --infoSR info
  closeHandle h
  return framesWritten

--Prototype UI, takes a signal and writes it to a file with dummy name
s :: String -> Signal Value -> IO Int
s name sig = do
  let info = defaultInfo 0 5 name
      tl = TimeLine sig info
  writeTL tl
  --reloadSC

testSig = \t ->
  switch t 0 0.3 * sin (2*pi* zto1 t 0 0.3) +
  switch t 0.3 0.9 * sin (8*pi* zto1 t 0.3 0.9) +
  switch t 0.9 1 * sin (16*pi* zto1 t 0.9 1)
  

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

reloadSC :: IO ()
reloadSC = do
  let m = OSC.Message "/TimeLines" [OSC.string "reload"]
  udp <- OSC.openUDP "127.0.0.1" 57120
  FD.sendOSC udp m



--send :: OSC.UDP -> OSC.Message -> IO ()
--send u m = OSC.sendOSC u m
