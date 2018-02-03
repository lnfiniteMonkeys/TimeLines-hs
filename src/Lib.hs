module Lib
    ( someFunc
    ) where

--import Control.Concurrent
import Sound.File.Sndfile as SF
import System.Directory
import System.IO as IO
import Foreign.Marshal.Array as MA
import Foreign.ForeignPtr as FP

someFunc :: IO ()
someFunc = do
  let rate = 10000
      frames = rate*60
      fileName = "testFile.w64"
      format = Format HeaderFormatW64 SampleFormatDouble EndianFile
      info = Info frames rate 1 format 1 True
      
  IO.openFile fileName IO.ReadWriteMode -- open/make empty file
  hout <- SF.openFile fileName SF.ReadWriteMode info -- open a write handle
  
  --buff <- SF.hGetBuffer hout frames
 -- writeNum <- SF.hPutBuf hout () 512
  
  let array = [sin x | x <- take frames [0, 1..]]::[Double]
  arrayPtr <- MA.newArray array
  --foreignPtr <- FP.newForeignPtr_ arrayPtr
  --buff <- SF.fromForeignPtr foreignPtr 0 1000
 -- report <- SF.hPutBuffer hout buff
  report <- SF.hPutBuf hout arrayPtr frames
  print report
  
  SF.hClose hout -- close handle
  return ()

