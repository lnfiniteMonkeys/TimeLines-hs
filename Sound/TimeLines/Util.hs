module Sound.TimeLines.Util where

import Prelude
import System.Directory
import Control.Exception
import System.IO.Error 
import System.Directory as DIR
import System.FilePath

import Foreign.Marshal.Array as MA
import Foreign.Ptr
import Foreign.ForeignPtr as FP
import qualified Sound.File.Sndfile as SF
import Prelude as Pr

import Data.Fixed
import Sound.TimeLines.Types

-- | Returns a list going from start to end in number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)

-- | Calculates the duration, in seconds, of a TLinfo
ftlDur :: FiniteTimeLine -> Time
ftlDur (FTL _ (s, e)) = e - s

getTimeDomain :: FiniteTimeLine -> [Time]
getTimeDomain ftl@(FTL _ (s, e)) = fromToIn s e $ ftlNumSteps ftl

ftlNumSteps :: FiniteTimeLine -> Int
ftlNumSteps ftl = Pr.floor $ ftlDur ftl * fromIntegral (ftlSR ftl)

-- | Samples the TimeLine according to its info, returns a list of values
getVals :: FiniteTimeLine -> [Value]
getVals ftl@(FTL (_, (sig, _))  _) = map f $ getTimeDomain ftl
  where f = runSig sig
        
--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
-- | Deletes a file if it already exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Returns the temp directory on the current OS
getTLTempDir :: IO FilePath
getTLTempDir = do
  osTemp <- DIR.getTemporaryDirectory
  let tlTemp = addTrailingPathSeparator $ joinPath [osTemp, "TimeLines", "buffers"]
  DIR.createDirectoryIfMissing True tlTemp
  return tlTemp

-- | Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: FilePath -> FiniteTimeLine -> IO SF.Handle
openHandle path ftl = SF.openFile path SF.ReadWriteMode info
        where format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (ftlNumSteps ftl) (ftlSR ftl) 1 format 1 True

-- | Samples a constant sig for t = 0
constSigToValue :: Signal a -> a
constSigToValue sig = runSig sig 0

-- | Closes a handle
closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- | Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: FiniteTimeLine -> IO (Ptr Value)
getArrayPtr ftl = MA.newArray $ getVals ftl
