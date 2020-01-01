module Sound.TimeLines.OSC where

  -- This module handles the OSC communication with SCLang
  -- and generally keeping track of IO
  
  import qualified Sound.OSC as OSC
  import qualified Sound.OSC.FD as FD
  
  import Data.IORef (IORef, newIORef, readIORef, writeIORef)
  import System.IO.Unsafe (unsafePerformIO)
  
  import Control.Concurrent
  import Control.Monad (void, forever, when)
  import Data.Maybe
  import Data.IORef
  
  import Sound.TimeLines.Types
  import Sound.TimeLines.Util
  import Sound.TimeLines.Globals
  
  -- set to true for OSC tracing
  debugMode = False
  
  makeStringMessage :: String -> [String] -> OSC.Message
  makeStringMessage path ss = OSC.Message path $ map OSC.string ss
  
  makeImmediateBundle :: [OSC.Message] -> OSC.Bundle
  makeImmediateBundle ms = OSC.Bundle OSC.immediately ms
    
  sendBundle :: OSC.Bundle -> IO ()
  sendBundle b = FD.sendBundle globalUDPRef b
  
  -- | Takes a path and an argument (both strings)
  -- | and sends them to SCLang
  sendStringMessage :: String -> String -> IO ()
  sendStringMessage path str = do
    FD.sendMessage globalUDPRef $ OSC.Message path [OSC.string str]
    when debugMode $ putStrLn $ "sent string message: " ++ path ++ " " ++ str
  
  sendIntMessage :: String -> Int -> IO ()
  sendIntMessage path i = do
    FD.sendMessage globalUDPRef $ OSC.Message path [OSC.int32 i]
    when debugMode $ putStrLn $ "sent Int message: " ++ path ++  " " ++ (show i)
    
  sendTestMessage :: IO ()
  sendTestMessage = sendStringMessage "TimeLines" "test"
  
  sendStringMessages :: String -> [String] -> IO ()
  sendStringMessages path strs = do
    let m = OSC.Message path $ map OSC.string strs
    FD.sendMessage globalUDPRef m
    when debugMode $ putStrLn $ "sent string messages: " ++ path ++  " " ++ (show strs)
    
  -- | The port at which SCLang is expecting communication
  -- | (default = 57120)
  scLangPort = 57120
  localPort  = 70000
  
  -- | Global reference of the UDP port used to
  -- | communicate with SCLang
  {-# NOINLINE globalUDPRef #-}
  globalUDPRef :: OSC.UDP
  globalUDPRef = unsafePerformIO $ OSC.openUDP "127.0.0.1" scLangPort
  
  udpServer :: (IO (), IO ()) ->  IO ()
  udpServer (aIncr, aEval) = void $ forkIO $ FD.withTransport s f
    where s = FD.udpServer "127.0.0.1" localPort
          f t = forever $ FD.recvMessage t >>= checkMessages (aIncr, aEval)
  
  checkMessages :: (IO (), IO ()) -> Maybe FD.Message -> IO ()
  checkMessages _ (Nothing) = return ()
  checkMessages (aIncr, aEval) (Just m) = case FD.messageAddress m of
    "/incrementWindow" -> aIncr
    "/evalSession" -> aEval
    _ -> print "Not matching"
  
  
