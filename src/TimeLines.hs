module TimeLines where

--import Control.Concurrent
import qualified Sound.File.Sndfile as SF
import System.IO as IO
import Foreign.Marshal.Array as MA
import Foreign.Ptr
import Foreign.ForeignPtr as FP
import System.Directory as D
import System.IO.Unsafe (unsafePerformIO)

import qualified Sound.OSC as OSC
import qualified Sound.OSC.FD as FD

import Signal
import Util

import Prelude as Pr

--
import Control.Concurrent (forkIO)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Reader
import Control.Monad.State

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


-- A TimeLine is made up of a signal (defined over infinite time starting from 0)
-- and a TLinfo, which contains the info needed to write a part of the signal to a file
data TimeLine = TimeLine {tlSig::Signal Value,
                          tlInfo::TLinfo
                         }



--data ParamList = ParamList {plParams :: [String],  }

--synth :: Synth -> (Time -> ParamList) -> IO ()
--synth sdef f = do
  






-- Duration of file to be written and played back
infDur :: TLinfo -> Time
infDur (TLinfo (s, e) _ _) = e - s

-- The number of frames that will be written to file
infNumFrames :: TLinfo -> Int
infNumFrames i = Pr.floor $ infDur i * fromIntegral (infSR i)


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
  where f = sigFunc sig
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
{-# NOINLINE globalWindowRef #-}
globalWindowRef :: IORef Window
globalWindowRef = unsafePerformIO $ newIORef (0, 1)

--Updates the global time Window
window :: Time -> Time -> IO Window
window s e = do
  writeIORef globalWindowRef (s, e)
  return (s, e)
  
--Renders all timelines with the new Window
--writeAllTimelines = undefined
--find which parameters are defined, get their signal, render it
--over the current window, reload buffers

--Render a timeline over the current window and write it to a file
writeParamFile :: String -> (Time -> Value) -> IO()
writeParamFile name sig = do
  currentWindow <- readIORef globalWindowRef
  let info = defaultInfo currentWindow name
  framesWritten <- writeTL $ TimeLine (Signal sig) info
  return ()

--Send SC a message to load a certain buffer and update the synth
sendUpdateMsg :: String -> IO()
sendUpdateMsg filename = sendMessage "/TimeLines/load" filename

--Read the context of a parameter (i.e. the synth name), write
--the timeline to a file with the appropriate name, and load it
sendParam :: String -> (Time -> Value) -> ReaderT String IO ()
sendParam p sig = do
  synthName <- ask
  let filename = "../buffers/"++synthName++"_"++p
  liftIO $ writeParamFile filename sig
  liftIO $ sendUpdateMsg filename

(<><) = sendParam

{-
synth' :: SynthName -> SynthDef -> StateT Synth IO() -> IO ()
synth' synthName synthDef params = do
  put (synthName, synthDef, []::ParamList)
  evalStateT params
-}  

type SynthDef = String
type SynthName = String
type Param = String
type ParamList = [Param]

type Synth = (SynthName, SynthDef, ParamList)



--
synth :: SynthName -> SynthDef -> ReaderT String IO() -> IO()
synth synthName synthDef params = do
  runReaderT params synthName



addParam :: Param -> StateT Synth IO ()
addParam p = do
  (sn, sd, pl) <- get
  put (sn, sd, p:pl)






{-
a |= b = synth a b 

type Args = String

type Synth = StateT (SynthName, SynthDef, [Args]) IO ()

snth :: SynthName -> SynthDef -> Synth

{-


every 
-}
{-
"perc" |= "fm" $ do
  "amp" <>< \t -> sin t
  "freq" <>< \t -> cos t
-}


-}
















sendPlay :: IO ()
sendPlay = do
  sendMessage "/TimeLines/play" ""

sendMessage path str = do
  let m = OSC.Message path [OSC.string str]
  udp <- OSC.openUDP "127.0.0.1" 57120
  FD.sendOSC udp m
