#!/usr/bin/env python3
import struct
import sys
import os

def read_wav_header(filename):
    """Read and parse WAV file header"""
    if not os.path.exists(filename):
        print(f"File {filename} does not exist")
        return False
        
    with open(filename, 'rb') as f:
        # Read RIFF header
        riff = f.read(4)
        if riff != b'RIFF':
            print(f"Invalid RIFF header: {riff}")
            return False
            
        file_size = struct.unpack('<I', f.read(4))[0]
        wave = f.read(4)
        if wave != b'WAVE':
            print(f"Invalid WAVE header: {wave}")
            return False
            
        # Read fmt chunk
        fmt = f.read(4)
        if fmt != b'fmt ':
            print(f"Invalid fmt chunk: {fmt}")
            return False
            
        fmt_size = struct.unpack('<I', f.read(4))[0]
        audio_format = struct.unpack('<H', f.read(2))[0]
        channels = struct.unpack('<H', f.read(2))[0]
        sample_rate = struct.unpack('<I', f.read(4))[0]
        byte_rate = struct.unpack('<I', f.read(4))[0]
        block_align = struct.unpack('<H', f.read(2))[0]
        bits_per_sample = struct.unpack('<H', f.read(2))[0]
        
        # Read data chunk
        data = f.read(4)
        if data != b'data':
            print(f"Invalid data chunk: {data}")
            return False
            
        data_size = struct.unpack('<I', f.read(4))[0]
        
        print(f"WAV File Analysis for {filename}:")
        print(f"  File size: {file_size + 8} bytes")
        print(f"  Audio format: {audio_format} ({'IEEE Float' if audio_format == 3 else 'PCM' if audio_format == 1 else 'Unknown'})")
        print(f"  Channels: {channels}")
        print(f"  Sample rate: {sample_rate} Hz")
        print(f"  Byte rate: {byte_rate}")
        print(f"  Block align: {block_align}")
        print(f"  Bits per sample: {bits_per_sample}")
        print(f"  Data size: {data_size} bytes")
        print(f"  Duration: {data_size / byte_rate:.2f} seconds")
        
        # Read a few samples to verify data
        samples = []
        for i in range(min(10, data_size // 4)):
            sample_bytes = f.read(4)
            if len(sample_bytes) == 4:
                sample = struct.unpack('<f', sample_bytes)[0]
                samples.append(sample)
        
        print(f"  First few samples: {samples[:5]}")
        print(f"  Sample range: {min(samples):.4f} to {max(samples):.4f}")
        
        return True

def main():
    files_to_check = ["test_output.wav", "test_interface.wav"]
    
    for filename in files_to_check:
        if os.path.exists(filename):
            print(f"\n{'='*50}")
            success = read_wav_header(filename)
            if success:
                print("✓ WAV file is valid")
            else:
                print("✗ WAV file is invalid")
        else:
            print(f"\nFile {filename} not found - skipping")

if __name__ == "__main__":
    main()