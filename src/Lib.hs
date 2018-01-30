module Lib
    ( someFunc
    ) where

--import Control.Concurrent
import Sound.File.Sndfile as SF
import System.Directory
import System.IO as IO


someFunc :: IO ()
someFunc = do
  let frames = 512
      fileName = "testFile.w64"
      info = Info frames 441000 1 format 1 True
      format = Format HeaderFormatW64 SampleFormatDouble EndianFile
      --format <- Format HeaderFormatW64 SampleFormatDouble EndianFile
      
  IO.openFile fileName IO.ReadWriteMode -- write empty file
  hout <- SF.openFile fileName SF.WriteMode info -- open a write handle
  print $ SF.duration info
  SF.hClose hout -- close handle
  return ()
