{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Monatone.MP3.Writer
  ( writeMP3Metadata
  , WriteError(..)
  , Writer
  ) where

import Control.Exception (catch, IOException)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Put
import Data.Bits ((.|.), shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
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

-- | Write metadata to MP3 file incrementally without loading the entire file
-- Takes optional AlbumArt separately since Metadata only stores AlbumArtInfo
writeMP3Metadata :: Metadata -> Maybe AlbumArt -> OsPath -> Writer ()
writeMP3Metadata metadata maybeAlbumArt filePath = do
  -- Open file in read/write mode
  result <- liftIO $ tryIO $ withBinaryFile filePath ReadWriteMode $ \handle -> do
    runExceptT $ writeMP3HandleIncremental metadata maybeAlbumArt handle
  case result of
    Left (e :: IOException) -> throwError $ WriteIOError $ T.pack $ show e
    Right (Left err) -> throwError err
    Right (Right ()) -> return ()
  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO action = catch (Right <$> action) (return . Left)

-- | Write MP3 metadata using a file handle incrementally
writeMP3HandleIncremental :: Metadata -> Maybe AlbumArt -> Handle -> Writer ()
writeMP3HandleIncremental metadata maybeAlbumArt handle = do
  -- Find where the audio data starts (after existing ID3v2 tag if present)
  audioDataOffset <- findAudioDataOffsetHandle handle

  -- Generate new ID3v2 tag
  newTagData <- generateID3v2Tag metadata maybeAlbumArt
  let newTagSize = fromIntegral $ L.length newTagData
  
  -- Get file size
  _ <- liftIO $ hFileSize handle
  
  -- Now we need to either insert or delete bytes depending on size difference
  let sizeDiff = newTagSize - audioDataOffset
  
  if sizeDiff == 0 then do
    -- Same size, just overwrite
    liftIO $ do
      hSeek handle AbsoluteSeek 0
      L.hPut handle newTagData
  else if sizeDiff > 0 then do
    -- Need to insert bytes
    insertBytesInFile handle sizeDiff audioDataOffset
    -- Write new tag
    liftIO $ do
      hSeek handle AbsoluteSeek 0
      L.hPut handle newTagData
  else do
    -- Need to delete bytes
    let bytesToDelete = negate sizeDiff
    -- Write new tag first
    liftIO $ do
      hSeek handle AbsoluteSeek 0
      L.hPut handle newTagData
    -- Then delete the extra space
    deleteBytesInFile handle bytesToDelete newTagSize

-- | Find the start of audio data by skipping existing ID3v2 tag using a handle
findAudioDataOffsetHandle :: Handle -> Writer Int
findAudioDataOffsetHandle handle = do
  -- Seek to beginning
  liftIO $ hSeek handle AbsoluteSeek 0
  
  -- Read first 10 bytes for ID3v2 header
  headerBytes <- liftIO $ BS.hGet handle 10
  
  if BS.length headerBytes < 10 then
    return 0
  else
    case BS.unpack (BS.take 3 headerBytes) of
      [0x49, 0x44, 0x33] -> do  -- "ID3"
        -- Parse the ID3v2 header to get tag size
        case BS.unpack (BS.drop 6 headerBytes) of
          [s1, s2, s3, s4] -> do
            -- ID3v2 size is stored as a syncsafe integer (28 bits)
            let tagSize = syncSafeToInt s1 s2 s3 s4
            return $ 10 + tagSize  -- Header (10 bytes) + tag data
          _ -> throwError $ CorruptedWrite "Invalid ID3v2 header"
      _ -> return 0  -- No ID3v2 tag, audio data starts at beginning

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
    
    -- Move data from offset to offset+size, working backwards to avoid overwriting
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
  let go remaining = do
        if remaining <= 0 then
          return ()
        else do
          let chunkSize = min (fromIntegral bufferSize) remaining
          -- Read from end of source region
          hSeek handle AbsoluteSeek (src + remaining - chunkSize)
          chunk <- BS.hGet handle (fromIntegral chunkSize)
          -- Write to end of dest region
          hSeek handle AbsoluteSeek (dest + remaining - chunkSize)
          BS.hPut handle chunk
          go (remaining - chunkSize)
  
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

-- | Convert syncsafe integer (4 bytes) to regular integer
syncSafeToInt :: Word8 -> Word8 -> Word8 -> Word8 -> Int
syncSafeToInt b1 b2 b3 b4 = 
  let s1 = fromIntegral b1 .&. 0x7F
      s2 = fromIntegral b2 .&. 0x7F
      s3 = fromIntegral b3 .&. 0x7F
      s4 = fromIntegral b4 .&. 0x7F
  in (s1 `shiftL` 21) .|. (s2 `shiftL` 14) .|. (s3 `shiftL` 7) .|. s4

-- | Convert regular integer to syncsafe integer (4 bytes)
intToSyncSafe :: Int -> (Word8, Word8, Word8, Word8)
intToSyncSafe n =
  let b1 = fromIntegral $ (n `shiftR` 21) .&. 0x7F
      b2 = fromIntegral $ (n `shiftR` 14) .&. 0x7F
      b3 = fromIntegral $ (n `shiftR` 7) .&. 0x7F
      b4 = fromIntegral $ n .&. 0x7F
  in (b1, b2, b3, b4)

-- | Generate complete ID3v2.4 tag
generateID3v2Tag :: Metadata -> Maybe AlbumArt -> Writer L.ByteString
generateID3v2Tag metadata maybeAlbumArt = do
  -- Generate all frames
  frames <- generateFrames metadata maybeAlbumArt
  let framesData = L.concat frames
      framesSize = fromIntegral $ L.length framesData
  
  -- Create ID3v2.4 header
  let (s1, s2, s3, s4) = intToSyncSafe framesSize
      header = runPut $ do
        putByteString "ID3"      -- Signature
        putWord8 4               -- Major version (2.4)
        putWord8 0               -- Revision version
        putWord8 0               -- Flags (no unsync, no extended header, etc.)
        putWord8 s1              -- Size as syncsafe integer
        putWord8 s2
        putWord8 s3
        putWord8 s4
  
  return $ header <> framesData

-- | Generate all ID3v2.4 frames for the metadata
generateFrames :: Metadata -> Maybe AlbumArt -> Writer [L.ByteString]
generateFrames metadata maybeAlbumArt = do
  -- Start with empty list
  frames0 <- return []
  
  -- Add text frames
  frames1 <- addTextFrame frames0 "TIT2" (title metadata)
  frames2 <- addTextFrame frames1 "TPE1" (artist metadata)  
  frames3 <- addTextFrame frames2 "TALB" (album metadata)
  frames4 <- addTextFrame frames3 "TPE2" (albumArtist metadata)
  frames5 <- addTextFrame frames4 "TCON" (genre metadata)
  frames6 <- addTextFrame frames5 "TPUB" (publisher metadata)
  
  -- Add comment frame (COMM has special structure)
  frames7 <- case comment metadata of
    Nothing -> return frames6
    Just c -> do
      commFrame <- generateCOMMFrame c
      return $ frames6 ++ [commFrame]
  
  -- Add numeric frames
  frames8 <- addNumericFrame frames7 "TRCK" (trackNumber metadata)
  frames9 <- addNumericFrame frames8 "TPOS" (discNumber metadata)
  frames10 <- addNumericFrame frames9 "TDRC" (year metadata)  -- TDRC for recording date in ID3v2.4

  -- Add additional metadata fields using TXXX frames
  frames11 <- addTXXXFrame frames10 "BARCODE" (barcode metadata)
  frames12 <- addTXXXFrame frames11 "CATALOGNUMBER" (catalogNumber metadata)
  frames13 <- addTXXXFrame frames12 "LABEL" (recordLabel metadata)
  frames14 <- addTXXXFrame frames13 "MusicBrainz Album Release Country" (releaseCountry metadata)
  frames15 <- addTXXXFrame frames14 "MusicBrainz Album Status" (releaseStatus metadata)
  frames16 <- addTXXXFrame frames15 "MusicBrainz Album Type" (releaseType metadata)

  -- Add date field (separate from year) if present
  frames17 <- addTextFrame frames16 "TDRC" (date metadata)

  -- Add album art frame if provided
  finalFrames <- case maybeAlbumArt of
    Nothing -> return frames17
    Just artData -> do
      apicFrame <- generateAPICFrame artData
      return $ apicFrame : frames17

  return finalFrames
  where
    -- Helper to add text frame if value is present
    addTextFrame :: [L.ByteString] -> ByteString -> Maybe Text -> Writer [L.ByteString]
    addTextFrame frameList frameId maybeText = case maybeText of
      Nothing -> return frameList
      Just text -> do
        frame <- generateTextFrame frameId text
        return $ frame : frameList
    
    -- Helper to add numeric frame if value is present
    addNumericFrame :: [L.ByteString] -> ByteString -> Maybe Int -> Writer [L.ByteString]
    addNumericFrame frameList frameId maybeNum = case maybeNum of
      Nothing -> return frameList
      Just num -> do
        frame <- generateTextFrame frameId (T.pack $ show num)
        return $ frame : frameList

    -- Helper to add TXXX frame if value is present
    addTXXXFrame :: [L.ByteString] -> Text -> Maybe Text -> Writer [L.ByteString]
    addTXXXFrame frameList description maybeText = case maybeText of
      Nothing -> return frameList
      Just text -> do
        frame <- generateTXXXFrame description text
        return $ frame : frameList

-- | Generate a text frame (TIT2, TPE1, TALB, etc.)
generateTextFrame :: ByteString -> Text -> Writer L.ByteString
generateTextFrame frameId text = do
  -- Encode text as UTF-8 with BOM
  let textBytes = BS.cons 0x03 $ TE.encodeUtf8 text  -- 0x03 = UTF-8 encoding
      frameSize = BS.length textBytes
      (s1, s2, s3, s4) = intToSyncSafe frameSize
      
  return $ runPut $ do
    putByteString frameId       -- Frame ID (4 bytes)
    putWord8 s1                 -- Frame size as syncsafe integer
    putWord8 s2
    putWord8 s3
    putWord8 s4
    putWord16be 0               -- Frame flags
    putByteString textBytes     -- Frame content (encoding byte + UTF-8 text)

-- | Generate APIC frame for album art
generateAPICFrame :: AlbumArt -> Writer L.ByteString
generateAPICFrame art = do
  let mimeBytes = TE.encodeUtf8 $ albumArtMimeType art
      descBytes = TE.encodeUtf8 $ albumArtDescription art
      imageData = albumArtData art
      
      -- Calculate frame content size
      frameSize = 1 + BS.length mimeBytes + 1 + 1 + BS.length descBytes + 1 + BS.length imageData
      (s1, s2, s3, s4) = intToSyncSafe frameSize
      
  return $ runPut $ do
    putByteString "APIC"        -- Frame ID
    putWord8 s1                 -- Frame size as syncsafe integer
    putWord8 s2
    putWord8 s3
    putWord8 s4
    putWord16be 0               -- Frame flags
    putWord8 0x03                           -- UTF-8 encoding
    putByteString mimeBytes                 -- MIME type
    putWord8 0x00                           -- Null terminator
    putWord8 (albumArtPictureType art)      -- Picture type
    putByteString descBytes                 -- Description
    putWord8 0x00                           -- Null terminator  
    putByteString imageData                 -- Image data

-- | Generate COMM frame for comments (has special structure)
generateCOMMFrame :: Text -> Writer L.ByteString
generateCOMMFrame commentText = do
  let textBytes = TE.encodeUtf8 commentText
      -- COMM structure: encoding + language (3 bytes) + short description (empty) + null + actual comment
      frameContent = BS.concat [
        BS.singleton 0x03,       -- UTF-8 encoding
        "eng",                   -- Language code (English)
        BS.singleton 0x00,       -- Empty short description + null terminator
        textBytes                -- Actual comment text
        ]
      frameSize = BS.length frameContent
      (s1, s2, s3, s4) = intToSyncSafe frameSize

  return $ runPut $ do
    putByteString "COMM"        -- Frame ID
    putWord8 s1                 -- Frame size as syncsafe integer
    putWord8 s2
    putWord8 s3
    putWord8 s4
    putWord16be 0               -- Frame flags
    putByteString frameContent  -- Frame content

-- | Generate TXXX frame for user-defined text information
generateTXXXFrame :: Text -> Text -> Writer L.ByteString
generateTXXXFrame description text = do
  let descBytes = TE.encodeUtf8 description
      textBytes = TE.encodeUtf8 text
      -- TXXX structure: encoding + description + null + text value
      frameContent = BS.concat [
        BS.singleton 0x03,       -- UTF-8 encoding
        descBytes,               -- Description
        BS.singleton 0x00,       -- Null terminator
        textBytes                -- Text value
        ]
      frameSize = BS.length frameContent
      (s1, s2, s3, s4) = intToSyncSafe frameSize

  return $ runPut $ do
    putByteString "TXXX"        -- Frame ID
    putWord8 s1                 -- Frame size as syncsafe integer
    putWord8 s2
    putWord8 s3
    putWord8 s4
    putWord16be 0               -- Frame flags
    putByteString frameContent  -- Frame content