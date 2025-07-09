#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Sound.TimeLines.WavWriter as SF
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO

-- Test the exact interface used by TimeLines
testInterface :: IO ()
testInterface = do
    putStrLn "Testing TimeLines interface compatibility..."
    
    -- Test parameters similar to what TimeLines uses
    let sampleRate = 44100
        numChannels = 1
        numFrames = 44100  -- 1 second of audio
        
    -- Create format and info structures
    let format = SF.Format SF.HeaderFormatWav SF.SampleFormatDouble SF.EndianFile
        info = SF.Info numFrames sampleRate numChannels format 1 True
    
    -- Open file using the TimeLines interface
    handle <- SF.openFile "test_interface.wav" SF.ReadWriteMode info
    
    -- Generate test data (simple sine wave)
    let samples = [sin (2 * pi * 440 * fromIntegral i / fromIntegral sampleRate) | i <- [0..numFrames-1]]
    
    -- Convert to array pointer
    arrayPtr <- newArray samples
    
    -- Write using the TimeLines interface
    written <- SF.hPutBuf handle arrayPtr numFrames
    
    -- Close using the TimeLines interface
    SF.hClose handle
    
    putStrLn $ "Successfully wrote " ++ show written ++ " samples using TimeLines interface"
    putStrLn "Test completed. File: test_interface.wav"

main :: IO ()
main = testInterface