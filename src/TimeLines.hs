module TimeLines  where

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
import Data.IORef
default (Double, Rational)

type Window = (Time, Time)

-- A timeline (file written to disk) has a start (> 0) and end point,
-- a samplerate and a string that denotes which parameter of the synth it controls
data TLinfo = TLinfo {infWindow::Window,
                      infSR::Int,
                      infParam::String
                     }
              deriving (Eq, Show)



-- Duration of file to be written and played back
infDur :: TLinfo -> Time
infDur (TLinfo (s, e) _ _) = e - s

-- The number of frames that will be written to file
infNumFrames :: TLinfo -> Int
infNumFrames i = Pr.floor $ infDur i * fromIntegral (infSR i)

-- A TimeLine is made up of a signal (defined over infinite time starting from 0)
-- and a TLinfo, which contains the info needed to write a part of the signal to a file
data TimeLine = TimeLine {tlSig::Signal Value,
                          tlInfo::TLinfo
                         }


defaultSampleRate = 700
defaultInfo :: Window -> String -> TLinfo
defaultInfo w argName = TLinfo w defaultSampleRate argName

--Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle i = SF.openFile filename SF.ReadWriteMode info
        where filename = infParam i ++ ".w64"
              format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (infNumFrames i) (infSR i) 1 format 1 True

closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- Render the TimeLine over the duration specified by the TLinfo
getVals :: TimeLine -> [Value]
getVals (TimeLine sig info@(TLinfo (s, e) _ _)) = map f domain
  where f = valueAt sig
        domain = fromToIn s e $ infNumFrames info


--Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: TimeLine -> IO (Ptr Value)
getArrayPtr tl = do
  ptr <- MA.newArray $ getVals tl
  return ptr


--Takes a TimeLine, writes it to a file, and returns number of frames written
writeTL :: TimeLine -> IO Int
writeTL tl@(TimeLine sig info) = do
  let fileName = infParam info ++ ".w64"
  --IO.openFile fileName IO.ReadWriteMode
  _ <- removeIfExists fileName -----------------better way?
  h <- openHandle info
  arrayPtr <- getArrayPtr tl
  framesWritten <- SF.hPutBuf h arrayPtr $ infNumFrames info --infoSR info
  closeHandle h
  return framesWritten

--keeping track of the time window to render each TimeLine over
globalRefWindow::IORef Window
globalRefWindow = undefined

--Prototype UI, takes a parameter name and a signal and writes it to a file
s :: String -> (Time -> Value) -> IO Int
s name sig = do
  currentWindow <- readIORef globalRefWindow
  let info = defaultInfo currentWindow name
      tl = TimeLine (Signal sig) info
  writeTL tl


reloadSC :: IO ()
reloadSC = do
  let m = OSC.Message "/TimeLines" [OSC.string "reload"]
  udp <- OSC.openUDP "127.0.0.1" 57120
  FD.sendOSC udp m



--send :: OSC.UDP -> OSC.Message -> IO ()
--send u m = OSC.sendOSC u m
