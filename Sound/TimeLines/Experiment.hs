module Sound.TimeLines.Experiment where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad
import Data.IORef
import System.IO.Unsafe

import Sound.TimeLines.Types
import Sound.TimeLines.Instruments
import Sound.TimeLines.Constants
import Sound.TimeLines.OSC
import Sound.TimeLines.TimeLines

import qualified Sound.OSC.FD as O

{-# NOINLINE sr #-}
sr :: IORef Int
sr = unsafePerformIO $ newIORef 700

evalControlSigs :: Time -> [ControlSignal] -> IO ()
evalControlSigs t s = mapM_ (forkIO . evalControlSig t) s

evalControlSig :: Time -> ControlSignal -> IO ()
evalControlSig t s = sendFloatMessage path value
  where path = concat
          ["/TimeLines/setSynthParam/", ctrlSynthID s, "/", ctrlParam s]
        value = sampleSig (ctrlSignal s) t


start = do
  let schedulerSampleRate = 20000
  --setupOSC
  O.time >>= writeSessionRef . emptySession
  print "The TimeLines session has started."


{- 
  emptyForkIO . forever $ do
    sess <- readSessionRef
    realTime <- O.time
    let startTime   = sessStartTime sess
        logicalTime = realTime - startTime
    --(threadsToStop, threadsToStart) <- checkControlThreads sess
    threadDelay $ floor $ 1000000/schedulerSampleRate
-}


infiniteSession :: Collector Action -> IO ()
infiniteSession as = do
  let newActions = collectList as
      --newSess = Session 
  prevSess <- readSessionRef
  stoppedThreads <- stopThreads prevSess newActions
  startedThreads <- startThreads prevSess newActions
  writeSessionRef $ updateSessThreads stoppedThreads startedThreads prevSess


updateSessThreads :: [ControlThread] -> [ControlThread] -> Session -> Session
updateSessThreads stopped started sess = undefined

stopThreads :: Session -> [Action] -> IO [ControlThread]
stopThreads = undefined

startThreads :: Session -> [Action] -> IO [ControlThread]
startThreads = undefined

startThread :: ControlSignal -> IO ControlThread
startThread cs@(ControlSignal param sr sig) = do
  id <- forkIO $ controlThreadLoop cs
  return $ ControlThread id cs

controlThreadLoop :: ControlSignal -> IO ()
controlThreadLoop cs@(ControlSignal _ sr _) = forever $ do
  relativeTime <- currSessRelativeTime
  evalControlSig relativeTime cs
  threadDelay $ floor $ 1000000/(fromIntegral sr)
  


currSessRelativeTime :: IO Time
currSessRelativeTime = do
  now <- O.time
  startTime <- readSessionRef >>= pure . sessStartTime
  return (startTime - now)

evalSignal :: Time -> String -> Signal Value -> IO ()
evalSignal t path sig = undefined --do
--  sendStringMessage path $ show $ runSig sig t
