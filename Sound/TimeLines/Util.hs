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

--Range list with number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)


--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

getTempDirectory :: IO FilePath
getTempDirectory = do
  osTemp <- DIR.getTemporaryDirectory
  let tlTemp = osTemp ++ "/TimeLines/buffers/"
  DIR.createDirectoryIfMissing True tlTemp
  return tlTemp

--Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle i = SF.openFile filename SF.ReadWriteMode info
        where filename = infParam i
              format = SF.Format SF.HeaderFormatW64 SF.SampleFormatDouble SF.EndianFile
              info = SF.Info (infNumFrames i) (infSR i) 1 format 1 True


closeHandle :: SF.Handle -> IO()
closeHandle = SF.hClose


-- Render the TimeLine over the duration specified by the TLinfo
getVals :: TimeLine a -> [a]
getVals (TimeLine sig info@(TLinfo (s, e) _ _)) = map (runSig sig) domain
  where f = runSig sig
        domain = fromToIn s e $ infNumFrames info


--Takes a TimeLine and returns a Pointer to an array of its values
getArrayPtr :: TimeLine Double -> IO (Ptr Double)
getArrayPtr tl = do
  ptr <- MA.newArray $ getVals tl
  return ptr


-- don't evaluate second term if first term is 0
(*.) :: (Num a, Eq a) => a -> a -> a
(*.) 0 _ = 0
(*.) a b = a * b

--shortcut to use with $ for RHS argument
mu = (*.)
