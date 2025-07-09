#!/usr/bin/env python3
"""
Minimal test to create a simple WAV file and verify the custom WAV writer is working.
This creates a WAV file manually to test the format.
"""

import struct
import math
import os

def create_test_wav(filename, duration=1.0, sample_rate=44100, frequency=440.0):
    """Create a simple sine wave WAV file"""
    
    # Generate sine wave samples
    num_samples = int(duration * sample_rate)
    samples = []
    for i in range(num_samples):
        t = i / sample_rate
        sample = math.sin(2 * math.pi * frequency * t)
        samples.append(sample)
    
    # WAV header constants
    RIFF = b'RIFF'
    WAVE = b'WAVE'
    FMT = b'fmt '
    DATA = b'data'
    
    # Format parameters
    audio_format = 3  # IEEE Float
    num_channels = 1
    bits_per_sample = 32
    byte_rate = sample_rate * num_channels * bits_per_sample // 8
    block_align = num_channels * bits_per_sample // 8
    
    # Data size calculations
    data_size = num_samples * num_channels * bits_per_sample // 8
    file_size = 36 + data_size
    
    with open(filename, 'wb') as f:
        # RIFF header
        f.write(RIFF)
        f.write(struct.pack('<I', file_size))
        f.write(WAVE)
        
        # fmt chunk
        f.write(FMT)
        f.write(struct.pack('<I', 16))  # fmt chunk size
        f.write(struct.pack('<H', audio_format))  # audio format (IEEE Float)
        f.write(struct.pack('<H', num_channels))  # number of channels
        f.write(struct.pack('<I', sample_rate))  # sample rate
        f.write(struct.pack('<I', byte_rate))  # byte rate
        f.write(struct.pack('<H', block_align))  # block align
        f.write(struct.pack('<H', bits_per_sample))  # bits per sample
        
        # data chunk
        f.write(DATA)
        f.write(struct.pack('<I', data_size))
        
        # Write samples as 32-bit IEEE floats
        for sample in samples:
            f.write(struct.pack('<f', sample))
    
    print(f"Created WAV file: {filename}")
    print(f"  Duration: {duration} seconds")
    print(f"  Sample rate: {sample_rate} Hz")
    print(f"  Frequency: {frequency} Hz")
    print(f"  Samples: {num_samples}")
    print(f"  File size: {os.path.getsize(filename)} bytes")

def verify_wav_format(filename):
    """Verify the WAV file format"""
    if not os.path.exists(filename):
        print(f"File {filename} does not exist")
        return False
    
    with open(filename, 'rb') as f:
        # Read and verify header
        riff = f.read(4)
        if riff != b'RIFF':
            print(f"Invalid RIFF header: {riff}")
            return False
        
        file_size = struct.unpack('<I', f.read(4))[0]
        wave = f.read(4)
        if wave != b'WAVE':
            print(f"Invalid WAVE header: {wave}")
            return False
        
        print(f"✓ Valid WAV file: {filename}")
        print(f"  File size: {file_size + 8} bytes")
        return True

def main():
    # Create a test WAV file
    test_file = "python_test.wav"
    create_test_wav(test_file, duration=0.5, frequency=440.0)
    
    # Verify it
    if verify_wav_format(test_file):
        print("✓ Test WAV file created and verified successfully")
    else:
        print("✗ Test WAV file verification failed")
    
    # Also check for any existing WAV files
    for wav_file in ["test_output.wav", "test_interface.wav"]:
        if os.path.exists(wav_file):
            print(f"\nChecking existing file: {wav_file}")
            verify_wav_format(wav_file)

if __name__ == "__main__":
    main()