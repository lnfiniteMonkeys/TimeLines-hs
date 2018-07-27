module Sound.TimeLines.OSC where

-- This module handles the OSC communication with SCLang
-- and generally keeping track of IO

import qualified Sound.OSC as OSC
import qualified Sound.OSC.FD as FD

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Sound.TimeLines.Types
import Sound.TimeLines.Util


-- | Takes a path and an argument (both strings)
-- | and sends them to SCLang
sendMessage :: String -> String -> IO()
sendMessage path str = do
  let m = OSC.Message path [OSC.string str]
  FD.sendOSC globalUDPRef m

-- | Sends a "reset" message to SCLang
reset :: IO()
reset = do
  sendMessage "/TimeLines/reset" ""
  (s, e) <- readIORef globalWindowRef
  let dur = e - s
  sendMessage "/TimeLines/window" (show dur)

-- | Sends a "play" message to SCLang
sendPlay :: IO ()
sendPlay = do
  sendMessage "/TimeLines/play" ""

-- | Updates the global time Window
window :: Signal Time -> Signal Time -> IO Window
window s e = do
  let  s' = constSigToValue s
       e' = constSigToValue e
       dur = e'- s'
  sendMessage "/TimeLines/window" $ show dur
  writeIORef globalWindowRef (s', e')
  return (s', e')

-- | Global reference of the time Window over which
-- | to render each TimeLine
{-# NOINLINE globalWindowRef #-}
-- the "NOINLINE" statement makes sure to never
-- replace globalWindowRef with its body, thus creating
-- another IORef
globalWindowRef :: IORef Window
globalWindowRef = unsafePerformIO $ newIORef (0, 1)

-- | The port at which SCLang is expecting communication
-- | (default = 57120)
scLangPort = 57120

-- | Global reference of the UDP port used to
-- | communicate with SCLang
{-# NOINLINE globalUDPRef #-}
globalUDPRef :: OSC.UDP
globalUDPRef = unsafePerformIO $ OSC.openUDP "127.0.0.1" scLangPort

