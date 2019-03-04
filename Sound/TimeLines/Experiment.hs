module Sound.TimeLines.Experiment where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad
import Data.IORef
import System.IO.Unsafe

import Sound.TimeLines.Types
import Sound.TimeLines.Instruments
import Sound.TimeLines.Constants
import Sound.TimeLines.OSC

import qualified Sound.OSC.FD as O

{-# NOINLINE sr #-}
sr :: IORef Int
sr = unsafePerformIO $ newIORef 700


setSR :: Int -> IO ()
setSR amt = modifyIORef' sr $ \_ -> amt

main = do
  startTime <- O.time
  void . forkIO . forever $ do
    sampleRate <- readIORef sr
    realTime <- O.time
    let logicalTime = realTime - startTime
    let sigAmp = 0.2 * (sin01 $ 2*pi*t*0.1)
        sigFreq = 432 * (melody phrygian $ wrap01 $ t/5)
    evalSignal logicalTime "/timelines/amp" sigAmp
    evalSignal logicalTime "/timelines/freq" sigFreq
    threadDelay $ floor $ 1000000/(fromIntegral sampleRate)


evalSignal :: Time -> String -> Signal Value -> IO ()
evalSignal t path sig = do
  sendStringMessage path $ show $ runSig sig t
