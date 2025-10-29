{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Monatone.OGG
  ( parseOGG
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import System.IO (Handle, IOMode(..), hSeek, SeekMode(..))
import System.OsPath
import System.File.OsPath (withBinaryFile)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Monatone.Metadata  
import Monatone.Types

-- | OGG page header is always 27 bytes (before segment table)
oggPageHeaderSize :: Int
oggPageHeaderSize = 27

-- | Parse OGG file efficiently - only read metadata pages
parseOGG :: OsPath -> Parser Metadata
parseOGG filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Read first page to check OGG signature
    firstHeader <- BS.hGet handle 27
    if BS.length firstHeader < 27 || BS.take 4 firstHeader /= "OggS"
      then return $ Left $ UnsupportedFormat "Not an OGG file"
      else do
        -- Parse pages until we find what we need
        hSeek handle AbsoluteSeek 0
        metadata <- parseOggPages handle (emptyMetadata OGG) False False
        return $ Right metadata
  
  case result of
    Left err -> throwError err
    Right m -> return m

-- | Parse OGG pages looking for Vorbis headers
parseOggPages :: Handle -> Metadata -> Bool -> Bool -> IO Metadata
parseOggPages handle metadata foundIdent foundComment
  -- Stop when we have both headers
  | foundIdent && foundComment = return metadata
  | otherwise = do
      -- Read page header
      headerBytes <- BS.hGet handle oggPageHeaderSize
      
      if BS.length headerBytes < oggPageHeaderSize
        then return metadata  -- EOF
        else do
          -- Verify OGG page signature
          if BS.take 4 headerBytes /= "OggS"
            then return metadata  -- Invalid page, stop
            else do
              -- Parse header to get segment table size
              let numSegments = fromIntegral $ BS.index headerBytes 26
              
              -- Read segment table
              segmentTable <- BS.hGet handle numSegments
              
              -- Calculate total page data size
              let pageDataSize = sum $ map fromIntegral $ BS.unpack segmentTable
              
              -- For the first few pages, read and check for Vorbis headers
              -- Vorbis headers are always in the first 3 pages
              if not foundIdent || not foundComment
                then do
                  pageData <- BS.hGet handle pageDataSize
                  
                  -- Check packet type
                  let (newMetadata, newFoundIdent, newFoundComment) = 
                        if "\x01vorbis" `BS.isPrefixOf` pageData && not foundIdent
                        then (parseVorbisInfo pageData metadata, True, foundComment)
                        else if "\x03vorbis" `BS.isPrefixOf` pageData && not foundComment  
                        then (parseVorbisComment pageData metadata, foundIdent, True)
                        else (metadata, foundIdent, foundComment)
                  
                  -- Continue to next page
                  parseOggPages handle newMetadata newFoundIdent newFoundComment
                else do
                  -- Skip this page's data since we have what we need
                  hSeek handle RelativeSeek (fromIntegral pageDataSize)
                  return metadata

-- | Parse Vorbis identification header (packet type 1)
parseVorbisInfo :: BS.ByteString -> Metadata -> Metadata
parseVorbisInfo bs metadata =
  if BS.length bs < 30  -- Minimum size for valid header
    then metadata
    else
      let lazyBs = L.fromStrict bs
      in case runGetOrFail (parseVorbisInfoGet metadata) (L.drop 7 lazyBs) of
        Left _ -> metadata
        Right (_, _, result) -> result

parseVorbisInfoGet :: Metadata -> Get Metadata
parseVorbisInfoGet metadata = do
  _ <- getWord32le  -- vorbisVersion
  audioChannels <- getWord8
  audioSampleRate <- getWord32le
  bitrateMaximum <- getWord32le
  bitrateNominal <- getWord32le
  bitrateMinimum <- getWord32le
  
  -- The nominal bitrate is the average bitrate
  let bitrate' = if bitrateNominal > 0
                then Just $ fromIntegral $ bitrateNominal `div` 1000
                else if bitrateMaximum > 0 && bitrateMinimum > 0
                then Just $ fromIntegral $ (bitrateMaximum + bitrateMinimum) `div` 2000
                else Nothing
  
  return $ metadata
    { audioProperties = AudioProperties
      { sampleRate = Just $ fromIntegral audioSampleRate
      , channels = Just $ fromIntegral audioChannels
      , bitrate = bitrate'
      , bitsPerSample = Nothing  -- Not in Vorbis info
      , duration = Nothing  -- Would need granule position from last page
      }
    }

-- | Parse Vorbis comment (packet type 3)
parseVorbisComment :: BS.ByteString -> Metadata -> Metadata
parseVorbisComment bs metadata =
  if BS.length bs < 7
    then metadata
    else
      let lazyBs = L.fromStrict bs
      in case runGetOrFail (parseVorbisCommentGet metadata) (L.drop 7 lazyBs) of
        Left _ -> metadata
        Right (_, _, result) -> result

parseVorbisCommentGet :: Metadata -> Get Metadata
parseVorbisCommentGet metadata = do
  -- Read vendor string length (little-endian 32-bit)
  vendorLength <- getWord32le
  -- Skip vendor string
  skip (fromIntegral vendorLength)
  
  -- Read number of comments
  numComments <- getWord32le
  
  -- Read each comment
  comments <- parseCommentList (fromIntegral numComments)
  
  -- Convert to HashMap for efficient lookup
  let tagMap = HM.fromList comments
  
  -- Extract standard fields
  return $ metadata
    { title = HM.lookup "TITLE" tagMap
    , artist = HM.lookup "ARTIST" tagMap
    , album = HM.lookup "ALBUM" tagMap
    , albumArtist = HM.lookup "ALBUMARTIST" tagMap
    , year = (HM.lookup "YEAR" tagMap >>= readInt)
             <|> (HM.lookup "DATE" tagMap >>= extractYearFromDate)
    , date = HM.lookup "DATE" tagMap
    , comment = HM.lookup "COMMENT" tagMap
    , genre = HM.lookup "GENRE" tagMap
    , trackNumber = HM.lookup "TRACKNUMBER" tagMap >>= readInt
    , totalTracks = HM.lookup "TRACKTOTAL" tagMap >>= readInt
    , discNumber = HM.lookup "DISCNUMBER" tagMap >>= readInt
    , totalDiscs = HM.lookup "DISCTOTAL" tagMap >>= readInt
    , releaseCountry = HM.lookup "RELEASECOUNTRY" tagMap
    , recordLabel = HM.lookup "LABEL" tagMap
    , catalogNumber = HM.lookup "CATALOGNUMBER" tagMap
    , barcode = HM.lookup "BARCODE" tagMap
    , releaseStatus = HM.lookup "RELEASESTATUS" tagMap
    , releaseType = HM.lookup "RELEASETYPE" tagMap
    , albumArtInfo = HM.lookup "METADATA_BLOCK_PICTURE" tagMap >>= parseVorbisPictureInfo
    , musicBrainzIds = MusicBrainzIds
      { mbTrackId = HM.lookup "MUSICBRAINZ_RELEASETRACKID" tagMap
      , mbRecordingId = HM.lookup "MUSICBRAINZ_TRACKID" tagMap
      , mbReleaseId = HM.lookup "MUSICBRAINZ_ALBUMID" tagMap
      , mbReleaseGroupId = HM.lookup "MUSICBRAINZ_RELEASEGROUPID" tagMap
      , mbArtistId = HM.lookup "MUSICBRAINZ_ARTISTID" tagMap
      , mbAlbumArtistId = HM.lookup "MUSICBRAINZ_ALBUMARTISTID" tagMap
      , mbWorkId = HM.lookup "MUSICBRAINZ_WORKID" tagMap
      , mbDiscId = HM.lookup "MUSICBRAINZ_DISCID" tagMap
      }
    }
  where
    parseCommentList :: Int -> Get [(Text, Text)]
    parseCommentList 0 = return []
    parseCommentList n = do
      -- Read comment length
      commentLength <- getWord32le
      -- Read comment data
      commentBytes <- getByteString (fromIntegral commentLength)
      -- Parse the comment (format: "KEY=value")
      let comment' = case BS.split 0x3D commentBytes of -- Split on '='
            (key:value:rest') -> 
              let keyText = T.toUpper $ TE.decodeUtf8With TEE.lenientDecode key
                  valueText = TE.decodeUtf8With TEE.lenientDecode (BS.intercalate "=" (value:rest'))
              in Just (keyText, valueText)
            _ -> Nothing
      rest <- parseCommentList (n - 1)
      return $ case comment' of
        Just c -> c : rest
        Nothing -> rest

-- | Parse Vorbis picture info (base64-encoded FLAC picture block, metadata only)
parseVorbisPictureInfo :: Text -> Maybe AlbumArtInfo
parseVorbisPictureInfo encodedData =
  case B64.decode (TE.encodeUtf8 encodedData) of
    Left _ -> Nothing
    Right pictureData -> parseFLACPictureBlockInfo pictureData
  where
    parseFLACPictureBlockInfo :: BS.ByteString -> Maybe AlbumArtInfo
    parseFLACPictureBlockInfo bs =
      let lazyBs = L.fromStrict bs
      in case runGetOrFail parsePictureInfo lazyBs of
        Left _ -> Nothing
        Right (_, _, artInfo) -> Just artInfo

    parsePictureInfo :: Get AlbumArtInfo
    parsePictureInfo = do
      pictureType <- getWord32be
      mimeLength <- getWord32be
      mimeType <- getByteString (fromIntegral mimeLength)
      descLength <- getWord32be
      description <- getByteString (fromIntegral descLength)
      _width <- getWord32be
      _height <- getWord32be
      _colorDepth <- getWord32be
      _numColors <- getWord32be
      pictureDataLength <- getWord32be
      -- Skip reading the actual picture data for performance
      -- skip (fromIntegral pictureDataLength)

      return $ AlbumArtInfo
        { albumArtInfoMimeType = TE.decodeUtf8With TEE.lenientDecode mimeType
        , albumArtInfoPictureType = fromIntegral pictureType
        , albumArtInfoDescription = TE.decodeUtf8With TEE.lenientDecode description
        , albumArtInfoSizeBytes = fromIntegral pictureDataLength
        }

-- | Extract year from DATE field (YYYY-MM-DD or just YYYY)
extractYearFromDate :: T.Text -> Maybe Int
extractYearFromDate dateText = 
  let yearStr = T.takeWhile (/= '-') dateText
  in readInt yearStr

