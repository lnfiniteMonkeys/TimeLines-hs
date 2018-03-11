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

--
import Control.Concurrent (forkIO)

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
infoNumFrames :: TLinfo -> Int
infoNumFrames i = Pr.floor $ tlDur i * fromIntegral (tlSR i)

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
              info = SF.Info (infoNumFrames i) (tlSR i) 1 format 1 True

closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- Render the TimeLine over the duration specified by the TLinfo
getVals :: TimeLine -> [Value]
getVals (TimeLine sig info@(TLinfo s e _ _)) = map f domain
  where f = valueAt sig
        domain = fromToIn s e $ infoNumFrames info


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
  framesWritten <- SF.hPutBuf h arrayPtr $ infoNumFrames info --infoSR info
  closeHandle h
  return framesWritten

--Prototype UI, takes a parameter name and a signal and writes it to a file
s :: String -> (Time -> Value) -> IO Int
s name sig = do
  let info = defaultInfo 0 5 name
      tl = TimeLine (Signal sig) info
  writeTL tl


reloadSC :: IO ()
reloadSC = do
  let m = OSC.Message "/TimeLines" [OSC.string "reload"]
  udp <- OSC.openUDP "127.0.0.1" 57120
  FD.sendOSC udp m



--send :: OSC.UDP -> OSC.Message -> IO ()
--send u m = OSC.sendOSC u m
