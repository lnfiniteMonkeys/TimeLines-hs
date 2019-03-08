module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC
import Sound.TimeLines.Globals (globalSessionRef)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void, when)
--import Control.Monad.Writer (Writer, execWriter, tell)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Numeric (showFFloat)

{- INTERFACE FUNCTIONS -}



-- fork session wrappers?
emptyForkIO = void . forkIO


-- pseudocode
updateSession :: Collector Action -> IO ()
updateSession as = do
  let prevThreads
      newThreads
      threadsToKill
      threadsToSpawn
  killThreads threadsToKill
  newThreads <- spawnThreads threadsToSpawn



-- | Interface function that groups params into Synths
synth :: SynthID -> Collector ControlSignal -> Collector Action
synth id ps = registerSynthAction $ Synth id $ collectList ps

-- | Ignores a synth
mute :: Collector Action -> Collector Action
mute s = register EmptyAction

-- TODO
solo = undefined

-- | Interface function that registers signals to a Synth
addParam :: SynthParam -> Signal Value -> Collector ControlSignal
addParam p sig = register $ ControlSignal p defaultSampleRate sig
(<><) = addParam

-- | Same as above, but with user-defined sampling rate
addParamSR :: SynthParam -> SampleRate -> Signal Value -> Collector ControlSignal
addParamSR p sr sig = register $ ControlSignal p sr sig
(<><<) = addParamSR

-- | Register a patch between two synths
patchSynths :: SynthID -> SynthID -> Collector Action
patchSynths src dst = registerPatchAction (src, dst)
(><>) = patchSynths

------------
isInfiniteSession :: Session -> Bool
isInfiniteSession s = sessMode s == InfiniteMode

isFiniteSession :: Session -> Bool
isFiniteSession = not . isInfiniteSession

readSessionRef :: IO Session
readSessionRef = readIORef globalSessionRef

writeSessionRef :: Session -> IO ()
writeSessionRef s = modifyIORef' globalSessionRef $ \_ -> s

incrementWindowBy :: Time -> Session -> Session
incrementWindowBy amt sess = undefined

reset :: IO ()
reset = do
  sendStringMessage "/TimeLines/resetSession" ""
  writeSessionRef defaultSession
 
------------
{- Eval functions -}

-- | Evaluate all actions in the current Session
-- | and update the server
evalSession :: Session -> IO ()
evalSession sess = do
  let synthNames = synthIDList sess
      synthsWithID = synthList sess
      patches = patchList sess
      modifiers = sessModifierList sess
  undefined



evalCurrSession :: IO ()
evalCurrSession = readIORef globalSessionRef >>= evalSession

sendWindow :: Window -> IO ()
sendWindow (s, e) = sendStringMessage "/TimeLines/setWindowDur" $ show $ e - s

-- | Write a synth's signal buffers, return all filepaths
evalSynth :: Window -> Synth -> IO [FilePath]
evalSynth w s = undefined
  
replaceActions :: [Action] -> Session -> Session
replaceActions newActions (Session st m as w) = undefined

--sendLoadAllSynths :: [(SynthID, )]
{- PRINT FUNCTIONS -}

printNumSynths :: IO ()
printNumSynths = do
  sess <- readIORef globalSessionRef
  putStrLn $ (++) "Number of running synths: " $ show $ length $ synthList sess

printSynths :: IO ()
printSynths = do
  sess <- readIORef globalSessionRef
  putStrLn $ "Running synths:" ++ (show $ synthIDList sess)

-- TODO: test
printPatches :: [Patch] -> IO ()
printPatches =
  print . map (\(s1, s2) -> show $ s1 ++ "->>" ++ s2 ++ " ")

printWindow :: IO ()
printWindow = undefined

printMode :: IO ()
printMode = undefined

pathToTemp :: FilePath
pathToTemp = unsafePerformIO getTLTempDir
