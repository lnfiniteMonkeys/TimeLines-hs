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

import Control.Monad.Writer (Writer, execWriter, tell)
import qualified Data.Map.Strict as Map

type Collection a = Writer [a] ()

gatherCollection :: Collection a -> [a]
gatherCollection = execWriter

register :: a -> Collection a
register a = tell [a]

registerSynth :: SynthWithID -> Collection Action
registerSynth = register . ActionSynth

registerPlug :: Plug -> Collection Action
registerPlug = register . ActionPlug

-- | Interface function that groups synths together and evaluates them
session :: Collection Action -> IO ()
session as = do
  modifyIORef' globalSessionRef $ replaceActions $ gatherCollection as
  evalSession

-- | A function that evaluates all synths in the current
-- | session over the current window
evalSession :: IO ()
evalSession = do
  sess@(Session actions w _) <- readIORef globalSessionRef
  let synths = synthList sess
      plugs = plugList sess
  evalSynths synths w
  evalPlugs plugs
  sendMessages "/TimeLines/synthNames" $ [fst s | s <- synths]

-- TODO
evalPlugs :: [Plug] -> IO ()
evalPlugs = undefined

evalSynths :: [SynthWithID] -> Window -> IO ()
evalSynths synths w = mapM_ (forkIO . flip evalSynthWithID w) synths

-- | Evaluate a synth, writing all its parameters to files
-- | and, once they're all written, sending the message to
-- | update on the server
evalSynthWithID :: SynthWithID -> Window -> IO ()
evalSynthWithID (synthID, synth) w = do
  filePaths <- mapConcurrently (writeSingleParam synthID w) synth
  sendLoadMsgs filePaths

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
paramInterface :: Param -> Signal Value -> Collection ParamSignal
paramInterface p sig = register (p, (sig, fromIntegral defaultSamplingRate))
(<><) = paramInterface

-- | Same as above but with user-defined sampling rate
paramInterfaceSR :: Param -> SamplingRate -> Signal Value -> Collection ParamSignal
paramInterfaceSR p sr sig = register (p, (sig, sr))
(<<><) = paramInterfaceSR

-- | Interface function that groups params
synth :: SynthID -> Collection ParamSignal -> Collection Action
synth id params = registerSynth synthWithID
  where synthWithID = (id, ps)
        ps = gatherCollection params

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
  
replaceActions :: [Action] -> Session -> Session
replaceActions newActions (Session _ w m) = Session newActions w m

replaceSessionWindow :: Window -> Session -> Session
replaceSessionWindow newWindow (Session as _ m) = Session as newWindow m

sendLoadMsgs :: [String] -> IO ()
sendLoadMsgs filepaths = sendMessages "/TimeLines/load" filepaths

printNumSynths :: IO ()
printNumSynths = do
  sess <- readIORef globalSessionRef
  putStrLn $ (++) "Number of running synths: " $ show $ length $ synthList sess

printPlugs :: IO ()
printPlugs = do
  sess <- readIORef globalSessionRef
  print $ plugList sess
  
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
