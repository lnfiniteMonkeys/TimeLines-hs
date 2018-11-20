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
sendMessage :: String -> String -> IO ()
sendMessage path str = do
  let m = OSC.Message path [OSC.string str]
  FD.sendOSC globalUDPRef m

sendTestMessage :: IO ()
sendTestMessage = sendMessage "TimeLines" "test"

sendMessages :: String -> [String] -> IO ()
sendMessages path strs = do
  let m = OSC.Message path $ map OSC.string strs
  FD.sendOSC globalUDPRef m

-- | Sends a "reset" message to SCLang
reset :: IO()
reset = do
  sendMessage "/TimeLines/reset" ""
  (s, e) <- readIORef globalWindowRef
  let dur = e - s
  sendMessage "/TimeLines/setWindow" (show dur)
  putStrLn "Server reset"

-- | The port at which SCLang is expecting communication
-- | (default = 57120)
scLangPort = 57120

-- | Global reference of the UDP port used to
-- | communicate with SCLang
{-# NOINLINE globalUDPRef #-}
globalUDPRef :: OSC.UDP
globalUDPRef = unsafePerformIO $ OSC.openUDP "127.0.0.1" scLangPort
