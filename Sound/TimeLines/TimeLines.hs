module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF

--import System.IO as IO
import Data.IORef (readIORef)

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC

--
import Control.Concurrent (forkIO, ThreadId)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Reader
import Control.Monad.State

import Data.Fixed

default (Double)


--Takes a TimeLine, writes it to a file, and returns number of frames written
writeTL :: TimeLine -> IO Int
writeTL tl@(TimeLine sig info) = do
  let fileName = infParam info
  _ <- removeFileIfExists fileName
  h <- openHandle info
  arrayPtr <- getArrayPtr tl
  framesWritten <- SF.hPutBuf h arrayPtr $ infNumFrames info --infoSR info
  closeHandle h
  return framesWritten


--Renders all timelines with the new Window
--writeAllTimelines = undefined
--find which parameters are defined, get their signal, render it
--over the current window, reload buffers

--Render a timeline over the current window and write it to a file
writeParamFile :: Param -> (Time -> Value) -> IO()
writeParamFile name sig = do
  currentWindow <- readIORef globalWindowRef
  let info = defaultInfo currentWindow name
--  DIR.createDirectoryIfMissing True getTempDir
  framesWritten <- writeTL $ TimeLine (Signal sig) info
  return ()

--Send SC a message to load a certain buffer and update the synth
sendUpdateMsg :: String -> IO()
sendUpdateMsg filename = sendMessage "/TimeLines/load" filename

--Read the context of a parameter (i.e. the synth name), write
--the timeline to a file with the appropriate name, and load it
sendParam :: Param -> (Time -> Value) -> ReaderT SynthID IO ThreadId
sendParam p sig = do
  synthName <- ask
  let filename = synthName ++ "_" ++ p ++ ".w64"
  -- Spawn a new thread to write the file and send the update message
  liftIO $ forkIO $ do
    writeParamFile filename sig
    sendUpdateMsg filename

(<><) = sendParam

--
synth :: SynthID -> ReaderT String IO ThreadId -> IO ThreadId
synth synthID params = do
  pathToTemp <- getTempDirectory
  runReaderT params $ pathToTemp ++ synthID

