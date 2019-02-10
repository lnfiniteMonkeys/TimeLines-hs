module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC (sendMessages, sendStringMessage, udpServer, sendResetMsg, sendBundle, makeImmediateBundle, makeStringMessage)
import Sound.TimeLines.Globals (globalSessionRef)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void, when)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Numeric (showFFloat)

{- INTERFACE FUNCTIONS -}

-- | A session in which the window is explicit and static
-- | (that window can be either looped or one-shot trigerred)
finiteSession :: Window -> Collector Action -> IO ()
finiteSession w as = do
  let newSess = Session (collectList as) w FiniteMode
  modifyIORef' globalSessionRef (\_ -> newSess)
  evalSession newSess

-- | A session in which time constantly and infinitely
-- | increases (with the option of resetting it at any point)
infiniteSession :: Collector Action -> IO ()
infiniteSession as = do
  (Session _ w prevMode) <- readSessionRef
  let a = do
        let newWindow = (0, windowStep)
            newSess = Session (collectList as) newWindow InfiniteMode
        sendLoop 0
        sendFreeSynths
        sendWindow newWindow
        evalSession newSess
        sendLoop 1
        sendResetTimer
        modifyIORef' globalSessionRef $ \_ -> newSess
      b = modifyIORef' globalSessionRef $
        \_ -> Session (collectList as) w InfiniteMode
  if prevMode == FiniteMode then a else b

sendFreeSynths :: IO ()
sendFreeSynths = sendStringMessage "/TimeLines/freeAllSynths" []

sendLoop :: Int -> IO ()
sendLoop x = sendStringMessage "/TimeLines/setLoop" $ show x

sendResetTimer :: IO ()
sendResetTimer = sendStringMessage "/TimeLines/resetTimer" ""

  {-
  -- Reset window if previous session was finite
  let newSess = Session (collectList as) newWindow InfiniteMode
      newWindow = case prevMode of
        FiniteMode -> (0, windowStep)
        InfiniteMode -> w
  modifyIORef' globalSessionRef (\_ -> newSess)
-}


-- | Interface function that groups params into Synths
synth :: SynthID -> Collector ParamSignal -> Collector Action
synth id params = registerSynthAction (id, collectList params)

-- | Ignores a synth
mute :: Collector Action -> Collector Action
mute s = register EmptyAction

-- TODO
solo = undefined

-- | Interface function that registers signals to a Synth
addParam :: Param -> Signal Value -> Collector ParamSignal
addParam p sig = registerParam (p, (sig, fromIntegral defaultSamplingRate))
(<><) = addParam

-- | Same as above, but with user-defined sampling rate
addParamSR :: Param -> SamplingRate -> Signal Value -> Collector ParamSignal
addParamSR p sr sig = registerParam (p, (sig, sr))
(<><<) = addParamSR

-- | Register a patch between two synths
patchSynths :: SynthID -> SynthID -> Collector Action
patchSynths src dst = registerPatchAction (src, dst)
(->>) = patchSynths

------------
{- UPDATE LOOP -}
-- | OSC receiving server
setupOSC :: IO ()
setupOSC = udpServer incrementAndEvalIfInfinite

incrementAndEvalIfInfinite :: IO ()
incrementAndEvalIfInfinite = do
  sess <- readSessionRef
  when (isInfiniteSession sess) $ do
    let newSess = incrementWindowBy windowStep sess
    writeSessionRef newSess
    evalSession newSess

isInfiniteSession :: Session -> Bool
isInfiniteSession (Session _ _ m) = m == InfiniteMode

isFiniteSession :: Session -> Bool
isFiniteSession = not . isInfiniteSession

readSessionRef :: IO Session
readSessionRef = readIORef globalSessionRef

writeSessionRef :: Session -> IO ()
writeSessionRef s = modifyIORef' globalSessionRef $ \_ -> s

-- | Fixed increment by which the window increases in an infinite session
windowStep :: Time
windowStep = 0.5

incrementWindowBy :: Time -> Session -> Session
incrementWindowBy amt (Session as (s, e) m) = 
  Session as (s+amt, e+amt) m

reset :: IO ()
reset = do
  (Session as _ m) <- readIORef globalSessionRef
  let w = case m of
        FiniteMode -> (0, 1)
        InfiniteMode -> (0, windowStep)
  writeIORef globalSessionRef $ Session [] w m
  --sendResetMsg
  
------------
{- Eval functions -}

-- | Evaluate all actions in the current Session
-- | and update the server
evalSession :: Session -> IO ()
evalSession sess@(Session as w m) = do
  let synthNames = synthIDList sess
      patches = patchList sess
  -- write all buffers and get list of lists of paths
  listsOfPaths <- mapConcurrently (evalSynthWithID w) $ synthList sess
  -- send all synths to be loaded
  sendBundle $ makeImmediateBundle $
    map (makeStringMessage "/TimeLines/loadSynthBuffers") listsOfPaths
  -- send patch order
  sendMessages "/TimeLines/synthOrder" $ sortPatches patches
  -- send list of active synths so that inactive ones can be freed
  sendMessages "/TimeLines/currentSynths" synthNames
  sendMessages "/TimeLines/patches" $ flattenPatches patches

showWindowStep :: String
showWindowStep = Numeric.showFFloat Nothing windowStep ""

evalCurrSession :: IO ()
evalCurrSession = readIORef globalSessionRef >>= evalSession

sendWindow :: Window -> IO ()
sendWindow (s, e) = sendStringMessage "/TimeLines/setWindowDur" $ show $ e - s

-- | Write a synth's signal buffers, return all filepaths
evalSynthWithID :: Window -> SynthWithID -> IO [FilePath]
evalSynthWithID w (synthID, synth) = do
  mapConcurrently (writeParam synthID w) synth


-- TODO: clean up
-- | Takes a param and a signal, evaluates it over the current window
-- | writes it to a file, and returns its filepath
writeParam :: SynthID -> Window -> ParamSignal -> IO FilePath
writeParam synthID w pSig@(p, (sig, sr)) = do
  let filePath = pathToTemp ++ synthID ++ "_" ++ p ++ ".w64"
      ftl = FTL pSig w
  _ <- removeFileIfExists filePath
  h <- openHandle filePath ftl
  arrayPtr <- getArrayPtr ftl
  framesWritten <- SF.hPutBuf h arrayPtr $ ftlNumSteps ftl
  closeHandle h
  return filePath
  
replaceActions :: [Action] -> Session -> Session
replaceActions newActions (Session _ w m) = Session newActions w m

sendLoadSynthBuffers :: [Param] -> IO ()
sendLoadSynthBuffers filepaths = sendMessages "/TimeLines/load" filepaths

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
printWindow = do
  (Session _ w _) <- readIORef globalSessionRef
  putStrLn $ (++) "Current window: " $ show w

printMode :: IO ()
printMode = do
  (Session _ _ m) <- readIORef globalSessionRef
  putStrLn $ (++) "Current mode: " $ show m
  
pathToTemp :: FilePath
pathToTemp = unsafePerformIO getTLTempDir
