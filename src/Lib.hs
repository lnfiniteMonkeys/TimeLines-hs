module Lib
    ( someFunc
    ) where

--import Control.Concurrent
import Sound.File.Sndfile as SF
import System.Directory
import System.IO as IO
import Foreign.Marshal.Array as MA
import Foreign.ForeignPtr as FP

import Signal


type Time = Double
type Value = Double
type Range = Integer

data TLinfo = TLinfo {duration::Double,
                      sampleRate::Int,
                      param::String
                     }
              deriving (Eq, Show)









{-

--getArray :: TLinfo -> (Time -> Value) -> [Value]
--getArray (TLinfo dur sr param) f =install .tar.xz
--  map f samples
--  where samples = let numFrames = round $ dur * (fromIntegral sr)
--                  in  map (\x -> f x) (take numFrames [0, 1..])



someFunc :: IO()
someFunc = do
  let rate = 300
      dur = 5
      fileName = "testFile.w64"
      format = Format HeaderFormatW64 SampleFormatDouble EndianFile
      info = Info frames rate 1 format 1 True
      tlInfo = TLInfo dur rate "cutoff"
      
  IO.openFile fileName IO.ReadWriteMode -- open/make empty file
  hout <- SF.openFile fileName SF.ReadWriteMode info -- open a write handle
  
  arrayPtr <- MA.newArray $ getArray tlInfo (\x -> sin x)
  report <- SF.hPutBuf hout arrayPtr (toInteger $ dur * rate)
  print report
  SF.hClose hout -- close handle
  return ()


-}


someFunc :: IO ()
someFunc = do
  let rate = 700
      frames = rate*60
      fileName = "testFile.w64"
      format = Format HeaderFormatW64 SampleFormatDouble EndianFile
      info = Info frames rate 1 format 1 True
      
  IO.openFile fileName IO.ReadWriteMode -- open/make empty file
  hout <- SF.openFile fileName SF.ReadWriteMode info -- open a write handle
  
  --buff <- SF.hGetBuffer hout frames
  -- writeNum <- SF.hPutBuf hout () 512
  
  --let function x = sin x * sin $ x*0.5
      --phasor = [x/frames | x <- take frames [0, 1..]::[Double]]
      --array = map function phasor

  let array = [2.0 * (sin x) | x <- take frames [0, 1..]]::[Double]
  --foreignPtr <- FP.newForeignPtr_ arrayPtr
  --buff <- SF.fromForeignPtr foreignPtr 0 1000
  --report <- SF.hPutBuffer hout buff
  arrayPtr <- MA.newArray array
  report <- SF.hPutBuf hout arrayPtr frames
  print report
  SF.hClose hout -- close handle
  return ()

  
  
