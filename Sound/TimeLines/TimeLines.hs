module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC (sendMessages, sendMessage)
--
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import System.IO.Unsafe (unsafePerformIO)

--import Data.Functor.Identity (Identity)
--import Control.Monad
import Control.Monad.Writer --(Writer, execWriter, tell)
--import Control.Monad.Reader
--import Control.Monad.State
--import Data.Fixed
import qualified Data.Map.Strict as Map

-- | Interface function that groups synths together and evaluates them
session :: Writer [SynthWithID] () -> IO ()
session synthWriters = do
  let newList = execWriter synthWriters
  modifyIORef' globalSessionRef $ replaceSessionSynths newList
  evalSession

-- | A function that evaluates all synths in the current
-- | session over the current window
evalSession :: IO ()
evalSession = do
  (Session synths w _) <- readIORef globalSessionRef
  mapM (forkIO . flip evalSynthWithID w) synths
  return ()

-- | Evaluate a synth, writing all its parameters to files
-- | and, once they're all written, sending the message to
-- | update on the server
evalSynthWithID :: SynthWithID -> Window -> IO ()
evalSynthWithID (synthID, synth) w = do
  filePaths <- mapConcurrently (writeSingleParam synthID w) synth
  sendLoadMsgs filePaths
  return ()

-- | Takes a param and a signal, evaluates it over the current window
-- | writes it to a file, and returns its filepath
writeSingleParam :: SynthID -> Window -> ParamSignal -> IO String
writeSingleParam synthID w pSig@(p, (sig, sr)) = do
  let filePath = pathToTemp ++ synthID ++ "_" ++ p ++ ".w64"
      ftl = FTL pSig w
  _ <- removeFileIfExists filePath
  h <- openHandle filePath ftl
  arrayPtr <- getArrayPtr ftl
  framesWritten <- SF.hPutBuf h arrayPtr $ ftlNumSteps ftl
  closeHandle h
  return filePath

-- | Interface function that registers signals to a synth
paramInterface :: Param -> Signal Value -> Writer [ParamSignal] ()
paramInterface p sig =  tell [(p, (sig, fromIntegral defaultSamplingRate))]
(<><) = paramInterface

-- | Same as above but with user-defined sampling rate
paramInterfaceSR :: Param -> SamplingRate -> Signal Value -> Writer [ParamSignal] ()
paramInterfaceSR p sr sig = tell [(p, (sig, sr))]
(<<><) = paramInterfaceSR

-- | Interface function that groups params
synth :: SynthID -> Writer [ParamSignal] () -> Writer [SynthWithID] ()
synth id params = tell [synthWithID]
  where synthWithID = (id, ps)
        ps = execWriter params

plugSynthTo :: SynthID -> SynthID -> IO ()
plugSynthTo src dst = sendMessages "TimeLines/plug" [src, dst]
(>>>) = plugSynthTo

-- | Update the session's window, re-evaluate all synths
-- | and update the server's timer
window :: Window -> IO ()
window w@(s, e) = do
  modifyIORef' globalSessionRef $ replaceSessionWindow w
  sendMessage "/TimeLines/setWindow" durStr
  evalSession
    where durStr = show $ e - s

-- | Same as above but without sending a message to the server
window' :: Window -> IO ()
window' w = do
  modifyIORef' globalSessionRef $ replaceSessionWindow w
  evalSession
  
replaceSessionSynths :: [SynthWithID] -> Session -> Session
replaceSessionSynths newList (Session _ w m) = Session newList w m

replaceSessionWindow :: Window -> Session -> Session
replaceSessionWindow newWindow (Session list _ m) = Session list newWindow m

sendLoadMsgs :: [String] -> IO ()
sendLoadMsgs filepaths = sendMessages "/TimeLines/load" filepaths

printNumSynths :: IO ()
printNumSynths = do
  (Session list _ _) <- readIORef globalSessionRef
  putStrLn $ (++) "Number of running synths: " $ show $ length list

printWindow :: IO ()
printWindow = do
  (Session _ w _) <- readIORef globalSessionRef
  putStrLn $ (++) "Current window: " $ show w

printMode :: IO ()
printMode = do
  (Session _ _ m) <- readIORef globalSessionRef
  putStrLn $ (++) "Current mode: " $ show m
  
pathToTemp :: FilePath
pathToTemp = unsafePerformIO getTLTempDir
