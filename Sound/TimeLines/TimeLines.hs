module Sound.TimeLines.TimeLines where

import qualified Sound.File.Sndfile as SF

import Sound.TimeLines.Types
import Sound.TimeLines.Util
import Sound.TimeLines.OSC (sendMessages, sendMessage, udpServer, sendResetMsg)
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
  let newSess = Session (executeCollector as) w FiniteMode
  writeIORef globalSessionRef newSess
  evalSession newSess

-- | A session in which time constantly and infinitely
-- | increases (with the option of resetting it at any point)
infiniteSession :: Collector Action -> IO ()
infiniteSession as = do
  (Session _ w prevMode) <- readSessionRef
  -- Reset window if previous session was finite
  let newSess = Session (executeCollector as) newWindow InfiniteMode
      newWindow = case prevMode of
        FiniteMode -> (0, windowStep)
        InfiniteMode -> w
  writeSessionRef newSess

-- | Interface function that groups params into Synths
synth :: SynthID -> Collector ParamSignal -> Collector Action
synth id params = registerSynthAction (id, executeCollector params)

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
    let newSess = incrementWindowBy sess windowStep
    writeSessionRef newSess
    evalSession newSess

isInfiniteSession :: Session -> Bool
isInfiniteSession (Session _ _ m) = m == InfiniteMode

isFiniteSession :: Session -> Bool
isFiniteSession = not . isInfiniteSession

readSessionRef :: IO Session
readSessionRef = readIORef globalSessionRef >>= return

writeSessionRef :: Session -> IO ()
writeSessionRef = writeIORef globalSessionRef

-- | Fixed increment by which the window increases in an infinite session
windowStep :: Time
windowStep = 0.5

incrementWindowBy :: Session -> Time -> Session
incrementWindowBy (Session as (s, e) m) amt = 
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
evalSession sess@(Session _ w m) = void . forkIO $ do
  _ <- evalAndSendSynths (synthList sess) w
  sendSynthNames sess
  evalAndSendPatches $ patchList sess
  case m of
    FiniteMode -> sendWindow w
    InfiniteMode -> sendMessage "/TimeLines/setWindow"
      $ Numeric.showFFloat Nothing windowStep ""

evalCurrSession :: IO ()
evalCurrSession = readIORef globalSessionRef >>= evalSession

evalAndSendPatches :: [Patch] -> IO ()
evalAndSendPatches p = do
  sendMessages "/TimeLines/synthOrder" $ sortPatches p
  sendMessages "/TimeLines/patches" $ flattenPatches p

flattenPatches :: [Patch] -> [SynthID]
flattenPatches [] = []
flattenPatches ((s1, s2):ps) = [s1, s2] ++ flattenPatches ps

evalAndSendSynths :: [SynthWithID] -> Window -> IO ()
evalAndSendSynths synths w = mapM_ (forkIO . flip evalSynthWithID w) synths

sendSynthNames :: Session -> IO ()
sendSynthNames s = sendMessages "/TimeLines/synthNames" $ synthIDList s

sendWindow :: Window -> IO ()
sendWindow (s, e) = sendMessage "/TimeLines/setWindow" $ show $ e - s

-- | Evaluate a synth, writing all its parameters to files
-- | and, once they're all written, sending the message to
-- | update on the server
-- evalSynthWithID :: SynthWithID -> Window -> SynthGroup -> IO ()
evalSynthWithID :: SynthWithID -> Window -> IO ()
evalSynthWithID (synthID, synth) w = do
  filePaths <- mapConcurrently (writeSingleParam synthID w) synth
  sendLoadMsgs filePaths

-- TODO: clean up
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


-- | Update the session's window, re-evaluate all synths
-- | and update the server's timer
window :: Window -> IO ()
window w@(s, e) = do
  modifyIORef' globalSessionRef $ replaceSessionWindow w
  sendMessage "/TimeLines/setWindow" durStr
  evalCurrSession
    where durStr = show $ e - s

-- | Same as above but without sending a message to the server
window' :: Window -> IO ()
window' w = do
  modifyIORef' globalSessionRef $ replaceSessionWindow w
  evalCurrSession
  
replaceActions :: [Action] -> Session -> Session
replaceActions newActions (Session _ w m) = Session newActions w m

sendLoadMsgs :: [String] -> IO ()
sendLoadMsgs filepaths = sendMessages "/TimeLines/load" filepaths


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

