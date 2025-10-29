{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Monatone.FLAC
  ( parseFLAC
  , parseVorbisComments
  , loadAlbumArtFLAC
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word
import System.IO (Handle, IOMode(..), hSeek, SeekMode(..))
import System.OsPath
import System.File.OsPath (withBinaryFile)

import Monatone.Metadata
import Monatone.Types

-- | FLAC file signature "fLaC" in bytes
flacSignature :: BS.ByteString
flacSignature = "fLaC"

-- | FLAC metadata block type constants
blockTypeStreamInfo, blockTypePadding, blockTypeApplication :: Word8
blockTypeSeekTable, blockTypeVorbisComment, blockTypeCueSheet, blockTypePicture :: Word8
blockTypeStreamInfo    = 0
blockTypePadding       = 1
blockTypeApplication   = 2
blockTypeSeekTable     = 3
blockTypeVorbisComment = 4
blockTypeCueSheet      = 5
blockTypePicture       = 6

-- | FLAC metadata block types
data BlockType 
  = StreamInfo     -- 0
  | Padding        -- 1  
  | Application    -- 2
  | SeekTable      -- 3
  | VorbisComment  -- 4
  | CueSheet       -- 5
  | Picture        -- 6
  | Reserved Word8
  deriving (Show, Eq)

-- | Block header info
data BlockHeader = BlockHeader
  { isLast :: Bool
  , blockType :: BlockType
  , blockLength :: Word32
  } deriving (Show, Eq)

-- | Parse FLAC file efficiently - only read metadata blocks, not entire file
parseFLAC :: OsPath -> Parser Metadata
parseFLAC filePath = do
  metadata <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Read and verify FLAC signature (4 bytes)
    sig <- BS.hGet handle 4
    if sig /= flacSignature
      then return $ Left $ CorruptedFile "Invalid FLAC signature"
      else do
        -- Parse metadata blocks one by one until we hit the last one
        Right <$> parseMetadataBlocks handle (emptyMetadata FLAC)
  
  case metadata of
    Left err -> throwError err
    Right m -> return m

-- | Parse metadata blocks from file handle (streaming)
parseMetadataBlocks :: Handle -> Metadata -> IO Metadata
parseMetadataBlocks handle metadata = do
  -- Read block header (4 bytes)
  headerBytes <- BS.hGet handle 4
  if BS.length headerBytes < 4
    then return metadata  -- EOF
    else do
      let header = parseBlockHeader headerBytes
      
      -- Process block based on type
      updatedMetadata <- case blockType header of
        StreamInfo -> do
          streamInfoData <- BS.hGet handle (fromIntegral $ blockLength header)
          return $ parseStreamInfo streamInfoData metadata
          
        VorbisComment -> do
          vorbisData <- BS.hGet handle (fromIntegral $ blockLength header)
          return $ parseVorbisCommentsBlock vorbisData metadata
          
        Picture -> do
          -- Parse picture block for album art
          pictureData <- BS.hGet handle (fromIntegral $ blockLength header)
          return $ parsePictureBlock pictureData metadata
              
        _ -> do
          -- Unknown/unneeded block type - skip it
          hSeek handle RelativeSeek (fromIntegral $ blockLength header)
          return metadata
      
      -- Stop if this was the last metadata block
      if isLast header
        then return updatedMetadata
        else parseMetadataBlocks handle updatedMetadata

-- | Parse block header from 4 bytes
parseBlockHeader :: BS.ByteString -> BlockHeader
parseBlockHeader bs =
  let firstByte = BS.index bs 0
      isLastBlock = (firstByte .&. 0x80) /= 0
      blockTypeNum = firstByte .&. 0x7F
      -- Next 3 bytes are block size (big-endian 24-bit integer)
      sizeByte1 = fromIntegral (BS.index bs 1) :: Word32
      sizeByte2 = fromIntegral (BS.index bs 2) :: Word32
      sizeByte3 = fromIntegral (BS.index bs 3) :: Word32
      size = (sizeByte1 `shiftL` 16) .|. (sizeByte2 `shiftL` 8) .|. sizeByte3
  in BlockHeader
    { isLast = isLastBlock
    , blockType = numberToBlockType blockTypeNum
    , blockLength = size
    }

-- | Convert number to block type
numberToBlockType :: Word8 -> BlockType
numberToBlockType t 
  | t == blockTypeStreamInfo    = StreamInfo
  | t == blockTypePadding       = Padding
  | t == blockTypeApplication   = Application
  | t == blockTypeSeekTable     = SeekTable
  | t == blockTypeVorbisComment = VorbisComment
  | t == blockTypeCueSheet      = CueSheet
  | t == blockTypePicture       = Picture
  | otherwise                   = Reserved t

-- | Parse StreamInfo block
parseStreamInfo :: BS.ByteString -> Metadata -> Metadata
parseStreamInfo bs metadata = 
  let lazyBs = L.fromStrict bs
  in runGet (parseStreamInfoGet metadata) lazyBs

parseStreamInfoGet :: Metadata -> Get Metadata
parseStreamInfoGet metadata = do
  -- Min block size (16 bits)
  _ <- getWord16be  -- minBlockSize
  -- Max block size (16 bits)  
  _ <- getWord16be  -- maxBlockSize
  -- Min frame size (24 bits)
  _ <- getWord24be  -- minFrameSize
  -- Max frame size (24 bits)
  _ <- getWord24be  -- maxFrameSize
  
  -- Sample rate (20 bits), channels (3 bits), bits per sample (5 bits), total samples (36 bits)
  -- This is packed into 8 bytes
  packed <- getWord64be
  
  let sampleRate' = fromIntegral ((packed `shiftR` 44) .&. 0xFFFFF)
      channels' = fromIntegral ((packed `shiftR` 41) .&. 0x7) + 1
      bitsPerSample' = fromIntegral ((packed `shiftR` 36) .&. 0x1F) + 1
      totalSamples = fromIntegral (packed .&. 0xFFFFFFFFF) :: Integer
      
  -- MD5 signature (16 bytes) - we'll skip this
  skip 16
  
  -- Calculate duration in seconds
  let duration' = if sampleRate' > 0 
                 then Just $ fromIntegral totalSamples `div` sampleRate'
                 else Nothing
  
  return $ metadata 
    { audioProperties = AudioProperties
      { sampleRate = Just sampleRate'
      , channels = Just channels'
      , bitsPerSample = Just bitsPerSample'
      , bitrate = Nothing  -- Will be calculated later if needed
      , duration = duration'
      }
    }
  where
    getWord24be :: Get Word32
    getWord24be = do
      b1 <- getWord8
      b2 <- getWord8
      b3 <- getWord8
      return $ (fromIntegral b1 `shiftL` 16) .|. 
               (fromIntegral b2 `shiftL` 8) .|. 
               fromIntegral b3

-- | Parse Vorbis Comments block
parseVorbisCommentsBlock :: BS.ByteString -> Metadata -> Metadata
parseVorbisCommentsBlock bs metadata = 
  let lazyBs = L.fromStrict bs
  in runGet (parseVorbisCommentsGet metadata) lazyBs

-- | Parse Vorbis Comments (for compatibility)
parseVorbisComments :: L.ByteString -> Metadata -> Parser Metadata
parseVorbisComments bs metadata = 
  return $ runGet (parseVorbisCommentsGet metadata) bs

-- | Parse Vorbis Comments using Get monad
parseVorbisCommentsGet :: Metadata -> Get Metadata
parseVorbisCommentsGet metadata = do
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
    , acoustidFingerprint = HM.lookup "ACOUSTID_FINGERPRINT" tagMap <|>
                           HM.lookup "acoustid_fingerprint" tagMap
    , acoustidId = HM.lookup "ACOUSTID_ID" tagMap <|>
                  HM.lookup "acoustid_id" tagMap
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
            (key:value:_) -> 
              let keyText = T.toUpper $ TE.decodeUtf8With TEE.lenientDecode key
                  valueText = TE.decodeUtf8With TEE.lenientDecode (BS.intercalate "=" (value:[]))
              in Just (keyText, valueText)
            _ -> Nothing
      rest <- parseCommentList (n - 1)
      return $ case comment' of
        Just c -> c : rest
        Nothing -> rest

-- | Parse Picture block according to FLAC specification
-- Only extracts metadata, not the actual image data (for performance)
parsePictureBlock :: BS.ByteString -> Metadata -> Metadata
parsePictureBlock bs metadata =
  let lazyBs = L.fromStrict bs
  in case runGetOrFail parsePictureInfo lazyBs of
    Left _ -> metadata
    Right (_, _, artInfo) -> metadata { albumArtInfo = Just artInfo }
  where
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
      -- Skip the actual picture data instead of reading it
      -- skip (fromIntegral pictureDataLength)

      return $ AlbumArtInfo
        { albumArtInfoMimeType = TE.decodeUtf8With TEE.lenientDecode mimeType
        , albumArtInfoPictureType = fromIntegral pictureType
        , albumArtInfoDescription = TE.decodeUtf8With TEE.lenientDecode description
        , albumArtInfoSizeBytes = fromIntegral pictureDataLength
        }

-- | Load album art from FLAC file (full binary data for writing)
loadAlbumArtFLAC :: OsPath -> Parser (Maybe AlbumArt)
loadAlbumArtFLAC filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Read and verify FLAC signature (4 bytes)
    sig <- BS.hGet handle 4
    if sig /= flacSignature
      then return $ Left $ CorruptedFile "Invalid FLAC signature"
      else do
        -- Search for Picture block
        Right <$> findPictureBlock handle

  case result of
    Left err -> throwError err
    Right maybeArt -> return maybeArt
  where
    findPictureBlock :: Handle -> IO (Maybe AlbumArt)
    findPictureBlock handle = do
      -- Read block header (4 bytes)
      headerBytes <- BS.hGet handle 4
      if BS.length headerBytes < 4
        then return Nothing  -- EOF
        else do
          let header = parseBlockHeader headerBytes

          -- Check if this is a Picture block
          if blockType header == Picture
            then do
              -- Parse the picture block with full data
              pictureData <- BS.hGet handle (fromIntegral $ blockLength header)
              return $ parsePictureBlockFull pictureData
            else do
              -- Skip this block and continue
              hSeek handle RelativeSeek (fromIntegral $ blockLength header)
              -- Stop if this was the last metadata block
              if isLast header
                then return Nothing
                else findPictureBlock handle

    parsePictureBlockFull :: BS.ByteString -> Maybe AlbumArt
    parsePictureBlockFull bs =
      let lazyBs = L.fromStrict bs
      in case runGetOrFail parsePictureData lazyBs of
        Left _ -> Nothing
        Right (_, _, art) -> Just art

    parsePictureData :: Get AlbumArt
    parsePictureData = do
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
      pictureData <- getByteString (fromIntegral pictureDataLength)

      return $ AlbumArt
        { albumArtMimeType = TE.decodeUtf8With TEE.lenientDecode mimeType
        , albumArtPictureType = fromIntegral pictureType
        , albumArtDescription = TE.decodeUtf8With TEE.lenientDecode description
        , albumArtData = pictureData
        }

-- | Extract year from DATE field (YYYY-MM-DD or just YYYY)
extractYearFromDate :: T.Text -> Maybe Int
extractYearFromDate dateText =
  let yearStr = T.takeWhile (/= '-') dateText
  in readInt yearStr