module Lib  where

--import Control.Concurrent
import Sound.File.Sndfile as SF
import System.IO as IO
import Foreign.Marshal.Array as MA
import Foreign.Ptr
import Foreign.ForeignPtr as FP
import System.Directory as D

--import Sound.OSC
import Signal
import Util


import Data.Fixed

default (Double, Rational)

data TLinfo = TLinfo {infoDur::Time,
                      infoSR::Int,
                      infoChannels::Int,
                      infoParam::String
                     }
              deriving (Eq, Show)

data TimeLine = TimeLine {tlSig::Signal Value,
                          tlInfo::TLinfo}


defaultSampleRate = 700
defaultInfo :: Time -> String -> TLinfo
defaultInfo dur name = TLinfo dur defaultSampleRate 1 name 

--Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle (TLinfo dur sr chan param) = SF.openFile filename SF.ReadWriteMode info
        where filename = param ++ ".w64"
              format = Format HeaderFormatW64 SampleFormatDouble EndianFile
              numFrames = floor $ dur * fromIntegral sr
              info = SF.Info numFrames sr 1 format 1 True

closeHandle :: SF.Handle -> IO()
closeHandle h = SF.hClose h

--Render the TimeLine over 
getVals :: TimeLine -> [Value]
getVals (TimeLine sig info@(TLinfo dur sr _ _)) = map f domain
  where f = valueAt sig
        numFrames = floor $ dur * fromIntegral sr
        domain = fromToIn 0 1 numFrames


--Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: TimeLine -> IO (Ptr Value)
getArrayPtr tl = do
  ptr <-  MA.newArray $ getVals tl
  return ptr
  
--Takes a TimeLine, writes it to a file, and returns number of frames written
writeTL :: TimeLine -> IO Int
writeTL tl@(TimeLine sig info) = do
  let fileName =  infoParam info ++ ".w64"
      numFrames = floor $ infoDur info * (fromIntegral $ infoSR info)
  --IO.openFile fileName IO.ReadWriteMode
  _ <- removeIfExists fileName -----------------
  h <- openHandle info
  arrayPtr <- getArrayPtr tl
  framesWritten <- SF.hPutBuf h arrayPtr numFrames --infoSR info
  closeHandle h
  return framesWritten

--Prototype UI, takes a time function and writes it to a file with dummy name
s :: String -> (Time -> Value) -> IO Int
s name sf = do
  let info = Lib.defaultInfo 10 name
      tl = TimeLine (Signal sf) info
  writeTL tl 

testSig = \t ->
  switch t 0 0.3 * sin (2*pi* zto1 t 0 0.3) +
  switch t 0.3 0.9 * sin (8*pi* zto1 t 0.3 0.9) +
  switch t 0.9 1 * sin (16*pi* zto1 t 0.9 1)
  


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
