module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF

--import System.IO as IO
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC

--
import Control.Concurrent (forkIO, ThreadId)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Fixed
import qualified Data.Map.Strict as Map


default (Value)
        
-- | Takes a TimeLine, writes it to a file, and returns number of frames written
writeTL :: TimeLine Double -> IO Int
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

-- | Samples a timeline over the current window and writes it to a file
writeParamFile :: Param -> Signal Value -> IO()
writeParamFile name sig = do
  currentWindow <- readIORef globalWindowRef
  let info = defaultInfo currentWindow name
--  DIR.createDirectoryIfMissing True getTempDir
  framesWritten <- writeTL $ TimeLine sig info
  return ()

-- | Sends a message to SCLang to load a certain file
-- | in a buffer and update the respective synth
sendUpdateMsg :: String -> IO()
sendUpdateMsg filename = sendMessage "/TimeLines/load" filename

-- | Reads the context of a parameter (i.e. the synth name), writes
-- | the timeline to a file with the appropriate name, and sends
-- | a "load" message to SCLang
sendParam :: Param -> Signal Value -> ReaderT SynthID IO ThreadId
sendParam p sig = do
  synthID <- ask
  let synthAndParam = synthID ++ "_" ++ p 
      filepath = pathToTemp ++ synthAndParam ++ ".w64"
  -- Spawn a new thread to write the file and send the update message
  liftIO $ do
    modifyIORef' signalMapRef (Map.insert filepath sig)
    writeAndSend (filepath, sig)

writeAndSend :: (String, Signal Value) -> IO ThreadId
writeAndSend (filepath, sig) = forkIO $ do
  writeParamFile filepath sig
  sendUpdateMsg filepath 

writeAllAndSend :: IO [ThreadId]
writeAllAndSend = do
  m <- readIORef signalMapRef
  mapM writeAndSend (Map.toList m)


-- | Convenience operator to be used while playing
(<><) = sendParam

{-|
Provides the context for a synth by means of
the SynthID. Each Signal provided reads the
SynthID and prepends it to its filename so
that SCLang knows where to apply it
-}
synth :: SynthID -> ReaderT SynthID IO ThreadId -> IO ThreadId
synth synthID param = forkIO $ do
  runReaderT param synthID
  return ()

pathToTemp :: FilePath
pathToTemp = unsafePerformIO getTempDirectory

{-# NOINLINE signalMapRef #-}
signalMapRef :: IORef (Map.Map String (Signal Value))
signalMapRef = unsafePerformIO $ newIORef $ Map.empty

getMapSize :: IO ()
getMapSize = do
  map <- readIORef signalMapRef
  print $ Map.size map
