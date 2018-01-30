module Lib
    ( someFunc
    ) where

--import Control.Concurrent
import Sound.File.Sndfile as SF


someFunc :: IO ()
someFunc = do
  let frames = 512
      path = "home/Code/bob.w64"
      info = Info frames 441000 1 format 1 True
      format = Format HeaderFormatW64 SampleFormatDouble EndianFile
      --format <- Format HeaderFormatW64 SampleFormatDouble EndianFile
  hout <- SF.openFile path SF.WriteMode info
  print $ SF.duration info
  SF.hClose hout
  return ()
