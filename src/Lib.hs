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

default (Double, Rational)

--type Range = Integer



data TLinfo = TLinfo {infoDur::Time,
                      infoSR::Int,
                      infoChannels::Int,
                      infoParam::String
                     }
              deriving (Eq, Show)

data TimeLine = TimeLine {tlSig::Signal Double,
                          tlInfo::TLinfo}


defaultSR = 700

defaultInfo :: Time -> String -> TLinfo
defaultInfo dur name = TLinfo dur defaultSR 1 name 

--Takes a TLinfo and returns the ReadWrite handle to a file
openHandle :: TLinfo -> IO SF.Handle
openHandle (TLinfo dur sr chan param) = SF.openFile filename SF.ReadWriteMode info
        where filename = param ++ ".w64"
              format = Format HeaderFormatW64 SampleFormatDouble EndianFile
              numFrames = floor $ dur * fromIntegral sr
              info = SF.Info numFrames sr 1 format 1 True



fromToIn :: (Fractional a, Enum a) => a -> a -> Int -> [a]
fromToIn lo hi steps = [lo, lo+step .. hi]
  where
    range = hi - lo
    step = range / (fromIntegral steps)

{-
getArray :: TimeLine -> [Value]
getArray (TimeLine (Signal f) info) = map f samples
  where samples = let numFrames = round $ dur * (fromIntegral $ infoSR info)
                  in  map (\x -> f x) (take numFrames [0, 1..])
-}

getArray :: TimeLine -> [Value]
getArray (TimeLine sig info@(TLinfo dur sr _ _)) = map f domain
  where f = valueAt sig
        numFrames = dur * fromIntegral sr
        step = 1/(dur * fromIntegral sr)
        domain = [0, 0+step .. 1]


writeTL :: TimeLine -> IO Int
writeTL tl@(TimeLine sig info) = do
  let fileName = infoParam info ++ ".w64"
  IO.openFile fileName IO.ReadWriteMode
  h <- openHandle info
  arrayPtr <- MA.newArray $ getArray tl
  framesWritten <- SF.hPutBuf h arrayPtr $ infoSR info
  SF.hClose h
  return  framesWritten
 

--writeSignal :: Signal Double -> TLinfo -> IO()


{-



writeSignal :: Signal Double -> TLinfo -> IO()
writeSignal (Signal sf) info =  do
        let numFrames = floor $ (infoDur info) * (fromIntegral $ infoSR info)
            domain =  map fromIntegral $ take numFrames [0::Int, 1..]
            array = map sf $ map (\t -> t * infoDur info / fromIntegral numFrames) domain
            fileName = infoParam info ++ ".w64"
            format = Format HeaderFormatW64 SampleFormatDouble EndianFile
            fileInfo = Info numFrames (infoSR info) 1 format 1 True
        IO.openFile fileName IO.ReadWriteMode
        hout <- SF.openFile fileName SF.ReadWriteMode fileInfo
        arrayPtr <- MA.newArray array
        framesWritten <- SF.hPutBuf hout arrayPtr numFrames
        print framesWritten
        SF.hClose hout
        return ()

write = do
  writeSignal testSignal testInfo

-}

{-




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

  
  
