module Sound.TimeLines.WavWriter where

import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import System.IO
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

-- | WAV file format constants
riffChunkId :: Word32
riffChunkId = 0x46464952  -- "RIFF"

waveFormat :: Word32
waveFormat = 0x45564157   -- "WAVE"

fmtChunkId :: Word32
fmtChunkId = 0x20746D66   -- "fmt "

dataChunkId :: Word32
dataChunkId = 0x61746164  -- "data"

-- | WAV format for IEEE float (32-bit)
wavFormatFloat :: Word16
wavFormatFloat = 3

-- | Handle structure for WAV writing
data WavHandle = WavHandle
    { wavFilePath :: FilePath
    , wavSampleRate :: Int
    , wavNumChannels :: Int
    , wavNumFrames :: Int
    , wavFileHandle :: Handle
    , wavBytesPerSample :: Int
    }

-- | Open a WAV file for writing with 32-bit float samples
openWavFile :: FilePath -> Int -> Int -> Int -> IO WavHandle
openWavFile path sampleRate numChannels numFrames = do
    h <- openBinaryFile path WriteMode
    let bytesPerSample = 4  -- 32-bit float
        handle = WavHandle path sampleRate numChannels numFrames h bytesPerSample
    
    -- Write WAV header
    writeWavHeader handle
    return handle

-- | Write WAV header to file
writeWavHeader :: WavHandle -> IO ()
writeWavHeader wavHandle = do
    let sr = fromIntegral $ wavSampleRate wavHandle
        nc = fromIntegral $ wavNumChannels wavHandle
        nf = fromIntegral $ wavNumFrames wavHandle
        bps = fromIntegral $ wavBytesPerSample wavHandle
        
        -- Calculate sizes
        dataSize = nf * nc * bps
        fileSize = dataSize + 36  -- 36 bytes for header without data chunk
        
        -- Format chunk data
        fmtChunkSize = 16
        byteRate = sr * nc * bps
        blockAlign = nc * bps
        bitsPerSample = bps * 8
        
        header = runPut $ do
            -- RIFF header
            putWord32le riffChunkId
            putWord32le (fromIntegral fileSize)
            putWord32le waveFormat
            
            -- fmt chunk
            putWord32le fmtChunkId
            putWord32le fmtChunkSize
            putWord16le wavFormatFloat
            putWord16le nc
            putWord32le sr
            putWord32le byteRate
            putWord16le blockAlign
            putWord16le (fromIntegral bitsPerSample)
            
            -- data chunk header
            putWord32le dataChunkId
            putWord32le dataSize
    
    BS.hPut (wavFileHandle wavHandle) header

-- | Write floating point samples to WAV file
writeFloatSamples :: WavHandle -> Ptr Double -> Int -> IO Int
writeFloatSamples wavHandle arrayPtr numSamples = do
    -- Convert doubles to 32-bit floats and write
    samples <- peekArray numSamples arrayPtr
    let floatSamples = map (realToFrac :: Double -> Float) samples
        byteString = runPut $ mapM_ putFloat32le floatSamples
    
    BS.hPut (wavFileHandle wavHandle) byteString
    return numSamples

-- | Close WAV file
closeWavFile :: WavHandle -> IO ()
closeWavFile wavHandle = hClose (wavFileHandle wavHandle)

-- | Info structure compatible with Sound.File.Sndfile
data Info = Info
    { infoFrames :: Int
    , infoSampleRate :: Int
    , infoChannels :: Int
    , infoFormat :: Format
    , infoSections :: Int
    , infoSeekable :: Bool
    }

-- | Format structure (simplified)
data Format = Format
    { formatHeader :: HeaderFormat
    , formatSample :: SampleFormat
    , formatEndian :: EndianFormat
    }

data HeaderFormat = HeaderFormatW64 | HeaderFormatWav
data SampleFormat = SampleFormatDouble | SampleFormatFloat  
data EndianFormat = EndianFile | EndianLittle | EndianBig

-- | File modes
data Mode = ReadWriteMode | WriteMode | ReadMode

-- | Open file with libsndfile-compatible interface
openFile :: FilePath -> Mode -> Info -> IO WavHandle
openFile path _ info = do
    let sr = infoSampleRate info
        nc = infoChannels info
        nf = infoFrames info
    openWavFile path sr nc nf

-- | Write buffer with libsndfile-compatible interface
hPutBuf :: WavHandle -> Ptr Double -> Int -> IO Int
hPutBuf = writeFloatSamples

-- | Close with libsndfile-compatible interface
hClose :: WavHandle -> IO ()
hClose = closeWavFile