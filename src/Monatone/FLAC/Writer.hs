{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Monatone.FLAC.Writer
  ( writeFLACMetadata
  , WriteError(..)
  , Writer
  ) where

import Control.Exception (catch, IOException)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ((.|.), shiftL, shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import System.IO hiding (withBinaryFile)
import System.OsPath
import System.File.OsPath (withBinaryFile)

import Monatone.Metadata

-- Re-define WriteError and Writer locally to avoid circular imports
data WriteError
  = WriteIOError Text
  | UnsupportedWriteFormat AudioFormat  
  | InvalidMetadata Text
  | CorruptedWrite Text
  deriving (Show, Eq)

type Writer = ExceptT WriteError IO

-- | Buffer size for file operations (64KB)
bufferSize :: Int
bufferSize = 65536

-- | Write metadata to FLAC file incrementally
-- Takes optional AlbumArt separately since Metadata only stores AlbumArtInfo
writeFLACMetadata :: Metadata -> Maybe AlbumArt -> OsPath -> Writer ()
writeFLACMetadata metadata maybeAlbumArt filePath = do
  -- Open file in read/write mode
  result <- liftIO $ tryIO $ withBinaryFile filePath ReadWriteMode $ \handle -> do
    runExceptT $ writeFLACHandleIncremental metadata maybeAlbumArt handle
  case result of
    Left (e :: IOException) -> throwError $ WriteIOError $ T.pack $ show e
    Right (Left err) -> throwError err
    Right (Right ()) -> return ()
  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO action = catch (Right <$> action) (return . Left)

-- | Write FLAC metadata using a file handle incrementally
writeFLACHandleIncremental :: Metadata -> Maybe AlbumArt -> Handle -> Writer ()
writeFLACHandleIncremental metadata maybeAlbumArt handle = do
  -- Verify FLAC signature
  liftIO $ hSeek handle AbsoluteSeek 0
  sig <- liftIO $ BS.hGet handle 4
  case BS.unpack sig of
    [0x66, 0x4C, 0x61, 0x43] -> pure ()  -- "fLaC"
    _ -> throwError $ CorruptedWrite "Invalid FLAC signature"

  -- Extract original STREAMINFO block for preservation (it's always first, 34 bytes)
  streamInfoHeader <- liftIO $ BS.hGet handle 4
  streamInfoData <- liftIO $ BS.hGet handle 34
  let originalStreamInfo = L.fromStrict $ BS.append streamInfoHeader streamInfoData

  -- Find where the audio data starts
  audioDataOffset <- findAudioDataOffsetHandle handle 4  -- Start after "fLaC"

  -- Generate new metadata blocks with preserved STREAMINFO
  newMetadataBlocks <- generateMetadataBlocks metadata maybeAlbumArt originalStreamInfo
  let newMetadataSize = fromIntegral $ L.length newMetadataBlocks
  
  -- Get file size
  _ <- liftIO $ hFileSize handle
  
  -- Calculate size difference (new metadata vs old metadata)
  let oldMetadataSize = audioDataOffset - 4  -- Subtract "fLaC" signature
  let sizeDiff = newMetadataSize - oldMetadataSize
  
  if sizeDiff == 0 then do
    -- Same size, just overwrite metadata blocks
    liftIO $ do
      hSeek handle AbsoluteSeek 4  -- Position after "fLaC"
      L.hPut handle newMetadataBlocks
  else if sizeDiff > 0 then do
    -- Need to insert bytes
    insertBytesInFile handle sizeDiff audioDataOffset
    -- Write new metadata blocks
    liftIO $ do
      hSeek handle AbsoluteSeek 4
      L.hPut handle newMetadataBlocks
  else do
    -- Need to delete bytes
    let bytesToDelete = negate sizeDiff
    -- Write new metadata first
    liftIO $ do
      hSeek handle AbsoluteSeek 4
      L.hPut handle newMetadataBlocks
    -- Then delete extra space
    deleteBytesInFile handle bytesToDelete (4 + newMetadataSize)

-- | Find where audio data starts by parsing metadata blocks
findAudioDataOffsetHandle :: Handle -> Int -> Writer Int
findAudioDataOffsetHandle handle currentOffset = do
  -- Seek to current position
  liftIO $ hSeek handle AbsoluteSeek (fromIntegral currentOffset)
  
  -- Read block header (4 bytes)
  headerBytes <- liftIO $ BS.hGet handle 4
  if BS.length headerBytes < 4 then
    return currentOffset
  else do
    let header = runGet parseBlockHeader (L.fromStrict headerBytes)
    let blockSize = fromIntegral (blockLength header)
    let nextOffset = currentOffset + 4 + blockSize
    
    if isLast header
      then return nextOffset  -- This was the last metadata block
      else findAudioDataOffsetHandle handle nextOffset

-- | Insert bytes into file at given offset
insertBytesInFile :: Handle -> Int -> Int -> Writer ()
insertBytesInFile handle size offset = do
  -- Get current file size
  fileSize <- liftIO $ hFileSize handle
  let moveSize = fileSize - fromIntegral offset
  
  if moveSize < 0 then
    throwError $ WriteIOError "Invalid offset for insert"
  else do
    -- First, extend the file
    liftIO $ hSetFileSize handle (fileSize + fromIntegral size)
    
    -- Move data from offset to offset+size, working backwards
    moveDataBackwards handle (fromIntegral offset) (fromIntegral $ offset + size) moveSize

-- | Delete bytes from file at given offset  
deleteBytesInFile :: Handle -> Int -> Int -> Writer ()
deleteBytesInFile handle size offset = do
  -- Get current file size
  fileSize <- liftIO $ hFileSize handle
  let moveSize = fileSize - fromIntegral offset - fromIntegral size
  
  if moveSize < 0 then
    throwError $ WriteIOError "Invalid size/offset for delete"
  else do
    -- Move data from offset+size to offset
    moveDataForwards handle (fromIntegral $ offset + size) (fromIntegral offset) moveSize
    
    -- Truncate the file
    liftIO $ hSetFileSize handle (fileSize - fromIntegral size)

-- | Move data backwards in file (for insertions)
moveDataBackwards :: Handle -> Integer -> Integer -> Integer -> Writer ()
moveDataBackwards handle src dest count = do
  let go remaining' = do
        if remaining' <= 0 then
          return ()
        else do
          let chunkSize = min (fromIntegral bufferSize) remaining'
          -- Read from end of source region
          hSeek handle AbsoluteSeek (src + remaining' - chunkSize)
          chunk <- BS.hGet handle (fromIntegral chunkSize)
          -- Write to end of dest region
          hSeek handle AbsoluteSeek (dest + remaining' - chunkSize)
          BS.hPut handle chunk
          go (remaining' - chunkSize)
  
  liftIO $ go count

-- | Move data forwards in file (for deletions)
moveDataForwards :: Handle -> Integer -> Integer -> Integer -> Writer ()
moveDataForwards handle src dest count = do
  let go moved = do
        if moved >= count then
          return ()
        else do
          let chunkSize = min (fromIntegral bufferSize) (count - moved)
          -- Read from source
          hSeek handle AbsoluteSeek (src + moved)
          chunk <- BS.hGet handle (fromIntegral chunkSize)
          -- Write to dest
          hSeek handle AbsoluteSeek (dest + moved)
          BS.hPut handle chunk
          go (moved + chunkSize)
  
  liftIO $ go 0

-- | FLAC metadata block header
data BlockHeader = BlockHeader
  { isLast :: Bool
  , blockType :: Word8
  , blockLength :: Word32
  } deriving (Show)

-- | Parse FLAC metadata block header
parseBlockHeader :: Get BlockHeader
parseBlockHeader = do
  firstByte <- getWord8
  let lastFlag = (firstByte .&. 0x80) /= 0
  let bType = firstByte .&. 0x7F
  
  -- Block length is 24 bits
  b1 <- getWord8
  b2 <- getWord8
  b3 <- getWord8
  let len = (fromIntegral b1 `shiftL` 16) .|. 
            (fromIntegral b2 `shiftL` 8) .|. 
            fromIntegral b3
  
  return $ BlockHeader lastFlag bType len

-- | Extract the original STREAMINFO block (already read from handle)
_extractStreamInfoBlock :: L.ByteString -> Writer L.ByteString
_extractStreamInfoBlock blockData = do
  if L.length blockData < 38  -- 4 byte header + 34 byte STREAMINFO
    then throwError $ CorruptedWrite "File too small for STREAMINFO block"
    else return $ L.take 38 blockData  -- Include header + data

-- | Generate new metadata blocks
generateMetadataBlocks :: Metadata -> Maybe AlbumArt -> L.ByteString -> Writer L.ByteString
generateMetadataBlocks metadata maybeAlbumArt originalStreamInfo = do
  -- Generate Vorbis comment block with metadata
  vorbisBlock <- generateVorbisCommentBlock metadata False

  -- Mark STREAMINFO as not-last (clear the last-block flag)
  let streamInfoNotLast = case L.unpack originalStreamInfo of
        (firstByte:rest) -> L.pack $ (firstByte .&. 0x7F) : rest  -- Clear the 0x80 bit
        _ -> originalStreamInfo

  -- Generate Picture block if album art is provided
  case maybeAlbumArt of
    Nothing -> do
      -- Mark Vorbis comment as last block
      let vorbisBlockLast = case L.unpack vorbisBlock of
            (firstByte:rest) -> L.pack $ (firstByte .|. 0x80) : rest  -- Set the 0x80 bit
            _ -> vorbisBlock
      return $ streamInfoNotLast <> vorbisBlockLast

    Just albumArt -> do
      -- Generate Picture block
      pictureBlock <- generatePictureBlock albumArt True

      -- Mark Vorbis comment as not-last
      let vorbisBlockNotLast = case L.unpack vorbisBlock of
            (firstByte:rest) -> L.pack $ (firstByte .&. 0x7F) : rest  -- Clear the 0x80 bit
            _ -> vorbisBlock

      return $ streamInfoNotLast <> vorbisBlockNotLast <> pictureBlock

-- | Generate Vorbis comment block
generateVorbisCommentBlock :: Metadata -> Bool -> Writer L.ByteString
generateVorbisCommentBlock metadata isLastBlock = do
  -- Create vendor string
  let vendor = "Monatone 0.1.0.0"
  let vendorBytes = TE.encodeUtf8 vendor
  let vendorLenBytes = runPut $ putWord32le $ fromIntegral $ BS.length vendorBytes
  
  -- Create comment list
  comments <- generateVorbisComments metadata
  let commentCount = length comments
  let commentCountBytes = runPut $ putWord32le $ fromIntegral commentCount
  
  -- Encode each comment
  let encodeComment (key, value) = 
        let text = key <> "=" <> value
            textBytes = TE.encodeUtf8 text
            lenBytes = runPut $ putWord32le $ fromIntegral $ BS.length textBytes
        in lenBytes <> L.fromStrict textBytes
  
  let encodedComments = L.concat $ map encodeComment comments
  
  -- Build complete Vorbis comment data
  let vorbisData = vendorLenBytes <> L.fromStrict vendorBytes <> 
                  commentCountBytes <> encodedComments
  
  -- Create block header
  let blockLen = fromIntegral $ L.length vorbisData :: Word32
  let headerByte = if isLastBlock then 0x84 else 0x04  -- Block type 4 = Vorbis comment
  let header = runPut $ do
        putWord8 headerByte
        -- Write 24-bit length
        putWord8 $ fromIntegral $ (blockLen `shiftR` 16) .&. 0xFF
        putWord8 $ fromIntegral $ (blockLen `shiftR` 8) .&. 0xFF
        putWord8 $ fromIntegral $ blockLen .&. 0xFF
  
  return $ header <> vorbisData

-- | Generate Vorbis comments from metadata
generateVorbisComments :: Metadata -> Writer [(Text, Text)]
generateVorbisComments metadata = do
  let comments = []
  
  -- Add standard tags
  let comments1 = case title metadata of
        Just t -> ("TITLE", t) : comments
        Nothing -> comments
  
  let comments2 = case artist metadata of
        Just a -> ("ARTIST", a) : comments1
        Nothing -> comments1
  
  let comments3 = case album metadata of
        Just a -> ("ALBUM", a) : comments2
        Nothing -> comments2
  
  let comments4 = case albumArtist metadata of
        Just aa -> ("ALBUMARTIST", aa) : comments3
        Nothing -> comments3
  
  let comments5 = case trackNumber metadata of
        Just n -> ("TRACKNUMBER", T.pack $ show n) : comments4
        Nothing -> comments4
  
  let comments6 = case discNumber metadata of
        Just n -> ("DISCNUMBER", T.pack $ show n) : comments5
        Nothing -> comments5
  
  let comments7 = case year metadata of
        Just y -> ("DATE", T.pack $ show y) : comments6
        Nothing -> comments6
  
  let comments8 = case genre metadata of
        Just g -> ("GENRE", g) : comments7
        Nothing -> comments7
  
  let comments9 = case comment metadata of
        Just c -> ("COMMENT", c) : comments8
        Nothing -> comments8
  
  let comments10 = case publisher metadata of
        Just p -> ("PUBLISHER", p) : comments9
        Nothing -> comments9
  
  -- Add MusicBrainz IDs
  let mbIds = musicBrainzIds metadata
  let comments11 = case mbRecordingId mbIds of
        Just mbId -> ("MUSICBRAINZ_TRACKID", mbId) : comments10
        Nothing -> comments10
  
  let comments12 = case mbReleaseId mbIds of
        Just mbId -> ("MUSICBRAINZ_ALBUMID", mbId) : comments11
        Nothing -> comments11
  
  let comments13 = case mbArtistId mbIds of
        Just mbId -> ("MUSICBRAINZ_ARTISTID", mbId) : comments12
        Nothing -> comments12
  
  let comments14 = case mbAlbumArtistId mbIds of
        Just mbId -> ("MUSICBRAINZ_ALBUMARTISTID", mbId) : comments13
        Nothing -> comments13
  
  let comments15 = case mbReleaseGroupId mbIds of
        Just mbId -> ("MUSICBRAINZ_RELEASEGROUPID", mbId) : comments14
        Nothing -> comments14

  -- Add additional metadata fields
  let comments16 = case date metadata of
        Just d -> ("DATE", d) : comments15
        Nothing -> comments15

  let comments17 = case barcode metadata of
        Just b -> ("BARCODE", b) : comments16
        Nothing -> comments16

  let comments18 = case catalogNumber metadata of
        Just cn -> ("CATALOGNUMBER", cn) : comments17
        Nothing -> comments17

  let comments19 = case recordLabel metadata of
        Just rl -> ("LABEL", rl) : comments18
        Nothing -> comments18

  let comments20 = case releaseCountry metadata of
        Just rc -> ("RELEASECOUNTRY", rc) : comments19
        Nothing -> comments19

  let comments21 = case releaseStatus metadata of
        Just rs -> ("RELEASESTATUS", rs) : comments20
        Nothing -> comments20

  let comments22 = case releaseType metadata of
        Just rt -> ("RELEASETYPE", rt) : comments21
        Nothing -> comments21

  return comments22

-- | Generate Picture block for album art
generatePictureBlock :: AlbumArt -> Bool -> Writer L.ByteString
generatePictureBlock art isLastBlock = do
  let mimeBytes = TE.encodeUtf8 $ albumArtMimeType art
      descBytes = TE.encodeUtf8 $ albumArtDescription art
      imageData = albumArtData art

      -- Build picture data according to FLAC spec
      pictureData = runPut $ do
        putWord32be $ fromIntegral $ albumArtPictureType art  -- Picture type
        putWord32be $ fromIntegral $ BS.length mimeBytes      -- MIME type length
        putByteString mimeBytes                               -- MIME type
        putWord32be $ fromIntegral $ BS.length descBytes      -- Description length
        putByteString descBytes                               -- Description
        putWord32be 0                                         -- Width (0 = unknown)
        putWord32be 0                                         -- Height (0 = unknown)
        putWord32be 0                                         -- Color depth (0 = unknown)
        putWord32be 0                                         -- Number of colors (0 = unknown)
        putWord32be $ fromIntegral $ BS.length imageData      -- Picture data length
        putByteString imageData                               -- Picture data

      blockLen = fromIntegral $ L.length pictureData :: Word32
      headerByte = if isLastBlock then 0x86 else 0x06  -- Block type 6 = Picture

      -- Build block header
      header = runPut $ do
        putWord8 headerByte
        -- Write 24-bit length
        putWord8 $ fromIntegral $ (blockLen `shiftR` 16) .&. 0xFF
        putWord8 $ fromIntegral $ (blockLen `shiftR` 8) .&. 0xFF
        putWord8 $ fromIntegral $ blockLen .&. 0xFF

  return $ header <> pictureData