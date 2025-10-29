{-# LANGUAGE OverloadedStrings #-}

-- | Main module for audio metadata parsing and representation.
-- 
-- This module provides the core types and functions for working with
-- audio metadata across different formats (FLAC, MP3, OGG/Vorbis, Opus).
module Monatone.Metadata
  ( AudioFormat(..)
  , Metadata(..)
  , AudioProperties(..)
  , MusicBrainzIds(..)
  , AlbumArtInfo(..)
  , AlbumArt(..)
  , emptyMetadata
  , emptyAudioProperties
  , emptyMusicBrainzIds
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Word (Word8)
import Data.Aeson

-- | Supported audio formats
data AudioFormat 
  = FLAC
  | OGG
  | Opus  
  | MP3
  deriving (Show, Eq, Ord, Read)

instance ToJSON AudioFormat where
  toJSON FLAC = "flac"
  toJSON OGG = "ogg"
  toJSON Opus = "opus"
  toJSON MP3 = "mp3"

instance FromJSON AudioFormat where
  parseJSON = withText "AudioFormat" $ \t -> case t of
    "flac" -> return FLAC
    "ogg" -> return OGG
    "opus" -> return Opus
    "mp3" -> return MP3
    _ -> fail $ "Unknown audio format: " ++ show t

-- | Audio file properties
data AudioProperties = AudioProperties
  { duration :: Maybe Int        -- Duration in milliseconds
  , bitrate :: Maybe Int         -- Bitrate in kbps (only for lossy formats like MP3)
  , sampleRate :: Maybe Int      -- Sample rate in Hz
  , channels :: Maybe Int        -- Number of channels
  , bitsPerSample :: Maybe Int   -- Bits per sample (bit depth)
  } deriving (Show, Eq)

-- | MusicBrainz identifiers
data MusicBrainzIds = MusicBrainzIds
  { mbRecordingId :: Maybe Text
  , mbTrackId :: Maybe Text
  , mbReleaseId :: Maybe Text
  , mbArtistId :: Maybe Text
  , mbAlbumArtistId :: Maybe Text  -- Album artist can differ from track artist
  , mbReleaseGroupId :: Maybe Text
  , mbWorkId :: Maybe Text          -- For classical works
  , mbDiscId :: Maybe Text          -- CD TOC-based disc ID
  } deriving (Show, Eq)

-- | Album art metadata without binary data (lightweight for scanning)
data AlbumArtInfo = AlbumArtInfo
  { albumArtInfoMimeType :: Text           -- e.g., "image/jpeg", "image/png"
  , albumArtInfoPictureType :: Word8       -- ID3v2 picture type (0 = Other, 3 = Cover Front, etc.)
  , albumArtInfoDescription :: Text        -- Textual description
  , albumArtInfoSizeBytes :: Int           -- Size of image data in bytes
  } deriving (Show, Eq)

-- | Album art / attached picture (includes full binary data)
data AlbumArt = AlbumArt
  { albumArtMimeType :: Text           -- e.g., "image/jpeg", "image/png"
  , albumArtPictureType :: Word8       -- ID3v2 picture type (0 = Other, 3 = Cover Front, etc.)
  , albumArtDescription :: Text        -- Textual description
  , albumArtData :: ByteString         -- Binary image data
  } deriving (Show, Eq)

-- | Complete metadata for an audio file
data Metadata = Metadata
  { format :: AudioFormat
  , title :: Maybe Text
  , artist :: Maybe Text
  , album :: Maybe Text
  , albumArtist :: Maybe Text
  , trackNumber :: Maybe Int
  , totalTracks :: Maybe Int      -- Total tracks in album
  , discNumber :: Maybe Int
  , totalDiscs :: Maybe Int       -- Total discs in album
  , date :: Maybe Text
  , year :: Maybe Int
  , genre :: Maybe Text
  , publisher :: Maybe Text
  , comment :: Maybe Text
  , releaseCountry :: Maybe Text  -- Country of release
  , recordLabel :: Maybe Text     -- Record label  
  , catalogNumber :: Maybe Text   -- Catalog number
  , barcode :: Maybe Text         -- Barcode/UPC
  , releaseStatus :: Maybe Text   -- Official, Promotional, etc.
  , releaseType :: Maybe Text     -- Album, Single, EP, etc.
  , albumArtInfo :: Maybe AlbumArtInfo  -- Album artwork metadata (lightweight)
  , audioProperties :: AudioProperties
  , musicBrainzIds :: MusicBrainzIds
  , acoustidFingerprint :: Maybe Text  -- Chromaprint/AcoustID fingerprint
  , acoustidId :: Maybe Text           -- AcoustID identifier
  , rawTags :: HashMap Text Text  -- All raw tags from the file
  } deriving (Show, Eq)

-- | Empty metadata with only format specified
emptyMetadata :: AudioFormat -> Metadata
emptyMetadata fmt = Metadata
  { format = fmt
  , title = Nothing
  , artist = Nothing
  , album = Nothing
  , albumArtist = Nothing
  , trackNumber = Nothing
  , totalTracks = Nothing
  , discNumber = Nothing
  , totalDiscs = Nothing
  , date = Nothing
  , year = Nothing
  , genre = Nothing
  , publisher = Nothing
  , comment = Nothing
  , releaseCountry = Nothing
  , recordLabel = Nothing
  , catalogNumber = Nothing
  , barcode = Nothing
  , releaseStatus = Nothing
  , releaseType = Nothing
  , albumArtInfo = Nothing
  , audioProperties = emptyAudioProperties
  , musicBrainzIds = emptyMusicBrainzIds
  , acoustidFingerprint = Nothing
  , acoustidId = Nothing
  , rawTags = HM.empty
  }

-- | Empty audio properties
emptyAudioProperties :: AudioProperties
emptyAudioProperties = AudioProperties
  { duration = Nothing
  , bitrate = Nothing
  , sampleRate = Nothing
  , channels = Nothing
  , bitsPerSample = Nothing
  }

-- | Empty MusicBrainz IDs
emptyMusicBrainzIds :: MusicBrainzIds
emptyMusicBrainzIds = MusicBrainzIds
  { mbRecordingId = Nothing
  , mbTrackId = Nothing
  , mbReleaseId = Nothing
  , mbArtistId = Nothing
  , mbAlbumArtistId = Nothing
  , mbReleaseGroupId = Nothing
  , mbWorkId = Nothing
  , mbDiscId = Nothing
  }