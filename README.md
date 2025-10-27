# Monatone

A pure Haskell library for parsing and writing audio metadata.

## Features

- **Pure Haskell** - No FFI dependencies or external binaries required
- **Multiple format support** - FLAC, MP3 (ID3v1/v2), OGG/Vorbis, and Opus
- **Read and write** - Full tag writing support for FLAC and MP3
- **Type-safe** - Strongly typed metadata representation
- **Efficient** - Streaming support for large files

## Installation

```bash
cabal install monatone
```

Or add to your `.cabal` file:

```cabal
build-depends: monatone >= 0.1.0
```

## Quick Start

```haskell
import Monatone.FLAC (parseMetadata)
import Monatone.Metadata

-- Parse metadata from a FLAC file
main :: IO ()
main = do
  result <- parseMetadata "song.flac"
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right metadata -> do
      putStrLn $ "Title: " ++ maybe "Unknown" id (title metadata)
      putStrLn $ "Artist: " ++ maybe "Unknown" id (artist metadata) 
      putStrLn $ "Album: " ++ maybe "Unknown" id (album metadata)
```

## Supported Formats

### FLAC
- Full Vorbis comment support (read/write)
- PICTURE block support for album art
- StreamInfo parsing for audio properties

### MP3
- ID3v1 tag support (read/write)
- ID3v2.3 and ID3v2.4 support (read/write)
- APIC frame support for album art
- Common frames: TIT2, TPE1, TALB, TYER, TCON, etc.

### OGG/Vorbis
- Vorbis comment reading
- Basic metadata extraction

### Opus
- Basic metadata support through Vorbis comments

## Writing Tags

```haskell
import Monatone.Writer
import Monatone.Types
import qualified Data.Map as Map

-- Update MP3 tags
updateMP3Tags :: FilePath -> IO ()
updateMP3Tags file = do
  let tags = Map.fromList
        [ ("TIT2", "New Title")
        , ("TPE1", "New Artist")
        , ("TALB", "New Album")
        ]
  result <- writeMP3Tags file tags
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right () -> putStrLn "Tags updated successfully"
```

## API Documentation

Full API documentation is available on [Hackage](https://hackage.haskell.org/package/monatone).

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests on [GitHub](https://github.com/rembo10/monatone).

## License

GPL 3.0 License - see LICENSE file for details.
