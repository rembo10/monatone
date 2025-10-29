{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Monatone.Common
  ( detectFormat
  , parseMetadata
  , loadAlbumArt
  ) where

import Control.Monad.Except (runExceptT)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO (IOMode(..))
import System.OsPath
import System.File.OsPath (withBinaryFile)

import Monatone.Metadata
import Monatone.Types
import qualified Monatone.FLAC as FLAC
import qualified Monatone.OGG as OGG
import qualified Monatone.MP3 as MP3

-- | Detect audio format from file header
detectFormat :: ByteString -> Maybe AudioFormat
detectFormat bs
  | BS.isPrefixOf "fLaC" bs = Just FLAC
  | BS.isPrefixOf "OggS" bs = detectOggFormat bs
  | hasMP3Header bs = Just MP3
  | otherwise = Nothing

-- | Detect specific OGG format (Vorbis vs Opus)
detectOggFormat :: ByteString -> Maybe AudioFormat
detectOggFormat bs
  | BS.isInfixOf "OpusHead" bs = Just Opus
  | BS.isInfixOf "vorbis" bs = Just OGG
  | otherwise = Just OGG  -- Default to OGG for unknown OGG streams

-- | Check for MP3 header (ID3 or sync frame)
hasMP3Header :: ByteString -> Bool
hasMP3Header bs
  | BS.length bs < 3 = False
  | BS.isPrefixOf "ID3" bs = True
  | BS.length bs >= 2 = 
      let firstByte = BS.index bs 0
          secondByte = BS.index bs 1
      in firstByte == 0xFF && (secondByte .&. 0xE0) == 0xE0
  | otherwise = False

-- | Parse metadata from file
parseMetadata :: OsPath -> IO (Either ParseError Metadata)
parseMetadata filePath = do
  -- Only read first 12 bytes for format detection
  header <- withBinaryFile filePath ReadMode $ \h -> BS.hGet h 12
  case detectFormat header of
    Nothing -> return $ Left $ UnsupportedFormat "Unknown audio format"
    Just fmt -> runExceptT $ case fmt of
      FLAC -> FLAC.parseFLAC filePath
      OGG -> OGG.parseOGG filePath
      Opus -> OGG.parseOGG filePath  -- Opus uses same OGG container format
      MP3 -> MP3.parseMP3 filePath

-- | Load full album art from file on-demand (for writing)
-- This reads only the album art data, not all metadata
loadAlbumArt :: OsPath -> IO (Either ParseError (Maybe AlbumArt))
loadAlbumArt filePath = do
  -- Only read first 12 bytes for format detection
  header <- withBinaryFile filePath ReadMode $ \h -> BS.hGet h 12
  case detectFormat header of
    Nothing -> return $ Left $ UnsupportedFormat "Unknown audio format"
    Just fmt -> runExceptT $ case fmt of
      FLAC -> FLAC.loadAlbumArtFLAC filePath
      OGG -> OGG.loadAlbumArtOGG filePath
      Opus -> OGG.loadAlbumArtOGG filePath
      MP3 -> MP3.loadAlbumArtMP3 filePath

