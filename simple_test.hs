#!/usr/bin/env runhaskell

-- Simple test to check if our modules can be imported
import Sound.TimeLines.WavWriter
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    putStrLn "Testing basic imports..."
    
    -- Test that we can create basic structures
    let format = Format HeaderFormatWav SampleFormatFloat EndianFile
        info = Info 1000 44100 1 format 1 True
    
    putStrLn "Basic structures created successfully"
    
    -- Test ByteString operations
    let testData = runPut $ do
            putWord32le 0x46464952  -- "RIFF"
            putWord32le 1000        -- file size
            putWord32le 0x45564157  -- "WAVE"
    
    putStrLn $ "Test ByteString length: " ++ show (BS.length testData)
    putStrLn "All basic operations work!"