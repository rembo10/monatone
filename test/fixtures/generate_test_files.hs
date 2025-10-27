#!/usr/bin/env bash
# Generate real test audio files using ffmpeg

echo "Generating real test audio files..."

# Generate 1 second of silence as raw PCM
ffmpeg -f lavfi -i anullsrc=r=44100:cl=stereo -t 1 -f s16le -y /tmp/silence.raw 2>/dev/null

# Create MP3 with metadata
ffmpeg -f s16le -ar 44100 -ac 2 -i /tmp/silence.raw \
  -codec:a libmp3lame -b:a 128k \
  -metadata title="Test Title" \
  -metadata artist="Test Artist" \
  -metadata album="Test Album" \
  -metadata date="2024" \
  -metadata track="1/10" \
  -metadata genre="Rock" \
  -metadata comment="Test comment" \
  -y test/fixtures/real_tagged.mp3 2>/dev/null

# Create minimal MP3 
ffmpeg -f s16le -ar 44100 -ac 2 -i /tmp/silence.raw \
  -codec:a libmp3lame -b:a 128k \
  -metadata title="Minimal Title" \
  -y test/fixtures/real_minimal.mp3 2>/dev/null

# Create FLAC with metadata
ffmpeg -f s16le -ar 44100 -ac 2 -i /tmp/silence.raw \
  -codec:a flac \
  -metadata title="FLAC Test Track" \
  -metadata artist="Test Band" \
  -metadata album="FLAC Album" \
  -metadata date="2024" \
  -metadata track="3" \
  -metadata comment="FLAC test comment" \
  -y test/fixtures/real_minimal.flac 2>/dev/null

# Clean up
rm -f /tmp/silence.raw

echo "Real test files generated:"
ls -lh test/fixtures/real_*.{mp3,flac} 2>/dev/null