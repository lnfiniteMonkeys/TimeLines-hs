module Util where

import Prelude
import System.Directory
import Control.Exception
import System.IO.Error 

import Data.Fixed
import Signal

--Range list with number of steps
fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step  = range / (fromIntegral steps)


--user ehird, https://stackoverflow.com/questions/8502201/remove-file-if-it-exists#8502391
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

