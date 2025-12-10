# Changelog for `monatone`

## [Unreleased]

### Added
- **M4A/AAC/ALAC Support**: Complete read/write support for M4A audio files
  - Full metadata parsing with iTunes-style tags (©nam, ©ART, aART, etc.)
  - Audio properties extraction (duration, sample rate, channels, bit depth)
  - AAC and Apple Lossless (ALAC) codec detection and parsing
  - Album art extraction (JPEG, PNG, BMP)
  - Track/disc number parsing with iTunes binary format
  - Complete metadata writing with atom reconstruction
  - **Freeform atom support** for MusicBrainz-style fields:
    - Record label (LABEL)
    - Catalog number (CATALOGNUMBER)
    - Barcode (BARCODE)
    - Release country (MusicBrainz Album Release Country)
    - All MusicBrainz IDs (recording, release, artist, album artist, release group, work, disc)
    - Acoustid fingerprint and ID
  - Integration with unified Writer API
  - Comprehensive test coverage

### Technical Details
- Pure Haskell MP4 atom parser with hierarchical structure support
- Efficient streaming implementation (no full file loading)
- Support for both 32-bit and 64-bit atom sizes
- Version-aware parsing (mvhd v0/v1, etc.)
- Safe file writing with temporary file approach
- Freeform (----) atom parsing and writing for custom metadata
- Zero compiler warnings

## [0.1.0.0] - Initial Release

### Added
- FLAC read/write support with Vorbis comments
- MP3 read/write support with ID3v1, ID3v2.3, and ID3v2.4 tags
- OGG/Vorbis and Opus basic support
- Album art handling
- Pure Haskell implementation
