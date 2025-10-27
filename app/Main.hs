{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Aeson (encode, ToJSON, object, (.=))
import Data.Aeson.Types (ToJSON(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.OsPath

import Monatone.Common (parseMetadata)
import Monatone.Metadata

-- Make Metadata instance for ToJSON 
instance ToJSON Metadata where
  toJSON metadata = object
    [ "format" .= format metadata
    , "title" .= title metadata
    , "artist" .= artist metadata
    , "album" .= album metadata
    , "albumArtist" .= albumArtist metadata
    , "trackNumber" .= trackNumber metadata
    , "totalTracks" .= totalTracks metadata
    , "discNumber" .= discNumber metadata
    , "totalDiscs" .= totalDiscs metadata
    , "date" .= date metadata
    , "year" .= year metadata
    , "genre" .= genre metadata
    , "publisher" .= publisher metadata
    , "comment" .= comment metadata
    , "releaseCountry" .= releaseCountry metadata
    , "recordLabel" .= recordLabel metadata
    , "catalogNumber" .= catalogNumber metadata
    , "barcode" .= barcode metadata
    , "releaseStatus" .= releaseStatus metadata
    , "releaseType" .= releaseType metadata
    , "albumArt" .= albumArt metadata
    , "audioProperties" .= audioProperties metadata
    , "musicBrainzIds" .= musicBrainzIds metadata
    , "acoustidFingerprint" .= acoustidFingerprint metadata
    , "acoustidId" .= acoustidId metadata
    , "rawTags" .= rawTags metadata
    ]

instance ToJSON AudioProperties where
  toJSON props = object
    [ "duration" .= duration props
    , "bitrate" .= bitrate props
    , "sampleRate" .= sampleRate props
    , "channels" .= channels props
    , "bitsPerSample" .= bitsPerSample props
    ]

instance ToJSON MusicBrainzIds where
  toJSON ids = object
    [ "mbRecordingId" .= mbRecordingId ids
    , "mbTrackId" .= mbTrackId ids
    , "mbReleaseId" .= mbReleaseId ids
    , "mbArtistId" .= mbArtistId ids
    , "mbAlbumArtistId" .= mbAlbumArtistId ids
    , "mbReleaseGroupId" .= mbReleaseGroupId ids
    , "mbWorkId" .= mbWorkId ids
    , "mbDiscId" .= mbDiscId ids
    ]

instance ToJSON AlbumArt where
  toJSON art = object
    [ "mimeType" .= albumArtMimeType art
    , "pictureType" .= albumArtPictureType art
    , "description" .= albumArtDescription art
    , "dataSize" .= BS.length (albumArtData art)
    -- Don't include base64 data in terminal output, just size
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePathStr] -> do
      putStrLn $ "Parsing file: " ++ filePathStr
      -- Convert string to OsPath
      filePath <- encodeFS filePathStr
      result <- parseMetadata filePath
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          exitFailure
        Right metadata -> do
          putStrLn "=== PARSED METADATA ==="
          L8.putStrLn $ encode metadata
          putStrLn ""
          putStrLn "=== HUMAN READABLE ==="
          printMetadata metadata
    _ -> do
      putStrLn "Usage: monatone <audio-file>"
      exitFailure

-- | Format track or disc info with total if available
formatTrackInfo :: Maybe Int -> Maybe Int -> Text
formatTrackInfo Nothing _ = "null"
formatTrackInfo (Just num) Nothing = T.pack $ show num
formatTrackInfo (Just num) (Just total) = T.pack $ show num ++ "/" ++ show total

printMetadata :: Metadata -> IO ()
printMetadata metadata = do
  putStrLn $ T.unpack $ "Format: " <> T.pack (show (format metadata))
  putStrLn $ T.unpack $ "Title: " <> fromMaybe "null" (title metadata)
  putStrLn $ T.unpack $ "Artist: " <> fromMaybe "null" (artist metadata)
  putStrLn $ T.unpack $ "Album: " <> fromMaybe "null" (album metadata)
  putStrLn $ T.unpack $ "Album Artist: " <> fromMaybe "null" (albumArtist metadata)
  putStrLn $ T.unpack $ "Track Number: " <> formatTrackInfo (trackNumber metadata) (totalTracks metadata)
  putStrLn $ T.unpack $ "Disc Number: " <> formatTrackInfo (discNumber metadata) (totalDiscs metadata)
  putStrLn $ T.unpack $ "Date: " <> fromMaybe "null" (date metadata)
  putStrLn $ T.unpack $ "Year: " <> maybe "null" (T.pack . show) (year metadata)
  putStrLn $ T.unpack $ "Genre: " <> fromMaybe "null" (genre metadata)
  putStrLn $ T.unpack $ "Publisher: " <> fromMaybe "null" (publisher metadata)
  putStrLn $ T.unpack $ "Comment: " <> fromMaybe "null" (comment metadata)
  putStrLn $ T.unpack $ "Release Country: " <> fromMaybe "null" (releaseCountry metadata)
  putStrLn $ T.unpack $ "Record Label: " <> fromMaybe "null" (recordLabel metadata)
  putStrLn $ T.unpack $ "Catalog Number: " <> fromMaybe "null" (catalogNumber metadata)
  putStrLn $ T.unpack $ "Barcode: " <> fromMaybe "null" (barcode metadata)
  putStrLn $ T.unpack $ "Release Status: " <> fromMaybe "null" (releaseStatus metadata)
  putStrLn $ T.unpack $ "Release Type: " <> fromMaybe "null" (releaseType metadata)
  
  putStrLn "\nAudio Properties:"
  let props = audioProperties metadata
  putStrLn $ T.unpack $ "  Duration: " <> maybe "null" (\d -> T.pack (show d) <> "ms") (duration props)
  putStrLn $ T.unpack $ "  Bitrate: " <> maybe "null" (\b -> T.pack (show b) <> " kbps") (bitrate props)
  putStrLn $ T.unpack $ "  Sample Rate: " <> maybe "null" (\s -> T.pack (show s) <> "Hz") (sampleRate props)
  putStrLn $ T.unpack $ "  Channels: " <> maybe "null" (T.pack . show) (channels props)
  putStrLn $ T.unpack $ "  Bits Per Sample: " <> maybe "null" (T.pack . show) (bitsPerSample props)
  
  putStrLn "\nMusicBrainz IDs:"
  let mbIds = musicBrainzIds metadata
  putStrLn $ T.unpack $ "  Recording ID: " <> fromMaybe "null" (mbRecordingId mbIds)
  putStrLn $ T.unpack $ "  Track ID: " <> fromMaybe "null" (mbTrackId mbIds)
  putStrLn $ T.unpack $ "  Release ID: " <> fromMaybe "null" (mbReleaseId mbIds)
  putStrLn $ T.unpack $ "  Artist ID: " <> fromMaybe "null" (mbArtistId mbIds)
  putStrLn $ T.unpack $ "  Album Artist ID: " <> fromMaybe "null" (mbAlbumArtistId mbIds)
  putStrLn $ T.unpack $ "  Release Group ID: " <> fromMaybe "null" (mbReleaseGroupId mbIds)
  putStrLn $ T.unpack $ "  Work ID: " <> fromMaybe "null" (mbWorkId mbIds)
  putStrLn $ T.unpack $ "  Disc ID: " <> fromMaybe "null" (mbDiscId mbIds)
  
  putStrLn "\nAcoustID:"
  putStrLn $ T.unpack $ "  Fingerprint: " <> fromMaybe "null" (acoustidFingerprint metadata)
  putStrLn $ T.unpack $ "  ID: " <> fromMaybe "null" (acoustidId metadata)
  
  putStrLn "\nAlbum Art:"
  case albumArt metadata of
    Nothing -> putStrLn "  None"
    Just art -> do
      putStrLn $ T.unpack $ "  MIME Type: " <> albumArtMimeType art
      putStrLn $ T.unpack $ "  Picture Type: " <> T.pack (show (albumArtPictureType art)) <> " (" <> describePictureType (albumArtPictureType art) <> ")"
      putStrLn $ T.unpack $ "  Description: " <> if T.null (albumArtDescription art) then "(empty)" else albumArtDescription art
      putStrLn $ T.unpack $ "  Data Size: " <> T.pack (show (BS.length (albumArtData art))) <> " bytes"
  
  putStrLn $ T.unpack $ "\nRaw tags count: " <> T.pack (show (length $ rawTags metadata))

describePictureType :: Word8 -> Text
describePictureType t = case t of
  0 -> "Other"
  1 -> "32x32 pixels 'file icon' (PNG only)"
  2 -> "Other file icon"
  3 -> "Cover (front)"
  4 -> "Cover (back)"
  5 -> "Leaflet page"
  6 -> "Media (e.g. label side of CD)"
  7 -> "Lead artist/lead performer/soloist"
  8 -> "Artist/performer"
  9 -> "Conductor"
  10 -> "Band/Orchestra"
  11 -> "Composer"
  12 -> "Lyricist/text writer"
  13 -> "Recording Location"
  14 -> "During recording"
  15 -> "During performance"
  16 -> "Movie/video screen capture"
  17 -> "A bright coloured fish"
  18 -> "Illustration"
  19 -> "Band/artist logotype"
  20 -> "Publisher/Studio logotype"
  _ -> "Unknown"
