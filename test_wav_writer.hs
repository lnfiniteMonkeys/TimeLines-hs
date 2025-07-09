#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Sound.TimeLines.WavWriter
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO

-- Test data: a simple sine wave
sampleRate :: Int
sampleRate = 44100

duration :: Double
duration = 1.0  -- 1 second

numSamples :: Int
numSamples = floor (duration * fromIntegral sampleRate)

-- Generate a sine wave at 440 Hz
generateSineWave :: Int -> Int -> [Double]
generateSineWave sr n = [sin (2 * pi * 440 * fromIntegral i / fromIntegral sr) | i <- [0..n-1]]

-- Test function
testWavWriter :: IO ()
testWavWriter = do
    putStrLn "Testing WAV writer..."
    
    -- Generate test data
    let samples = generateSineWave sampleRate numSamples
    
    -- Write to WAV file
    wavHandle <- openWavFile "test_output.wav" sampleRate 1 numSamples
    
    -- Convert to array pointer
    arrayPtr <- newArray samples
    
    -- Write samples
    written <- writeFloatSamples wavHandle arrayPtr numSamples
    
    -- Close file
    closeWavFile wavHandle
    
    putStrLn $ "Successfully wrote " ++ show written ++ " samples to test_output.wav"
    putStrLn "Test completed. You can play the file with any audio player."

main :: IO ()
main = testWavWriter