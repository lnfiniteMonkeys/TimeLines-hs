module Sound.TimeLines.Util where

import Prelude
import System.Directory
import Control.Exception
import System.IO.Error 
import System.Directory as DIR
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

-- | Samples the TimeLine according to its info, returns a list of values
getVals :: TimeLine a -> [a]
getVals (TimeLine sig info@(TLinfo (s, e) _ _)) = map f domain
  where f = runSig sig
        domain = fromToIn s e $ infNumFrames info
        
--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
-- | Deletes a file if it already exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Returns the temp directory on the current OS
getTempDirectory :: IO FilePath
getTempDirectory = do
  osTemp <- DIR.getTemporaryDirectory
  let tlTemp = osTemp ++ "/TimeLines/buffers/"
  DIR.createDirectoryIfMissing True tlTemp
  return tlTemp

-- | Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle i = SF.openFile filename SF.ReadWriteMode info
        where filename = infParam i
              format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (infNumFrames i) (infSR i) 1 format 1 True

-- | Samples a constant sig for t = 0
constSigToValue :: Signal a -> a
constSigToValue sig = runSig sig 0

-- | Closes a handle
closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose

-- | Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: TimeLine Double -> IO (Ptr Double)
getArrayPtr tl = do
  ptr <- MA.newArray $ getVals tl
  return ptr
