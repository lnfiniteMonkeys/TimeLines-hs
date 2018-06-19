module Sound.TimeLines.OSC where

import qualified Sound.OSC as OSC
import qualified Sound.OSC.FD as FD

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Sound.TimeLines.Types


sendMessage :: String -> String -> IO()
sendMessage path str = do
  let m = OSC.Message path [OSC.string str]
  FD.sendOSC globalUDPRef m

-- Send reset message
reset :: IO()
reset = do
  sendMessage "/TimeLines/reset" ""
  (s, e) <- readIORef globalWindowRef
  let dur = e - s
  sendMessage "/TimeLines/window" (show dur)

sendPlay :: IO ()
sendPlay = do
  sendMessage "/TimeLines/play" ""



--Updates the global time Window
window :: Time -> Time -> IO Window
window s e = do
  sendMessage "/TimeLines/window" $ show dur
  writeIORef globalWindowRef (s, e)
  return (s, e)
    where dur = e-s

--keeping track of the time window to render each TimeLine over
{-# NOINLINE globalWindowRef #-}
globalWindowRef :: IORef Window
globalWindowRef = unsafePerformIO $ newIORef (0, 1)

{-# NOINLINE globalUDPRef #-}
globalUDPRef :: OSC.UDP
globalUDPRef = unsafePerformIO $ OSC.openUDP "127.0.0.1" 57120
