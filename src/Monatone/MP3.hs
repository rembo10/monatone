{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Monatone.MP3
  ( parseMP3
  , loadAlbumArtMP3
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Data.Word
import System.IO (Handle, IOMode(..), hSeek, SeekMode(..), hFileSize, hTell)
import System.OsPath
import System.File.OsPath (withBinaryFile)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Monatone.Metadata
import Monatone.Types

-- | ID3v2 signature "ID3" in bytes
id3v2Signature :: BS.ByteString
id3v2Signature = "ID3"

-- | Parse MP3 file efficiently - only read ID3 tags and frame headers
parseMP3 :: OsPath -> Parser Metadata
parseMP3 filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Parse ID3v2 tag at beginning if present
    metadata <- parseID3v2FromHandle handle (emptyMetadata MP3)
    
    -- Parse ID3v1 tag at end if present (last 128 bytes)
    metadataWithId3v1 <- parseID3v1FromHandle handle metadata
    
    -- Parse MP3 audio properties from first frame
    audioProps <- parseMP3AudioPropertiesFromHandle handle
    
    return $ Right $ metadataWithId3v1 { audioProperties = audioProps }
  
  case result of
    Left err -> throwError err
    Right m -> return m

-- | Parse ID3v2 tag from file handle if present
parseID3v2FromHandle :: Handle -> Metadata -> IO Metadata
parseID3v2FromHandle handle metadata = do
  -- Read first 10 bytes (ID3v2 header)
  hSeek handle AbsoluteSeek 0
  header <- BS.hGet handle 10
  
  if BS.length header < 10 || BS.take 3 header /= id3v2Signature
    then return metadata  -- No ID3v2 tag
    else do
      -- Parse header to get tag size
      let version = BS.index header 3
          _minorVersion = BS.index header 4
          flags = BS.index header 5
          -- Size is synchsafe integer (7 bits per byte)
          size = parseSynchsafeInt (BS.drop 6 header)
      
      -- Read only the ID3v2 tag data
      tagData <- BS.hGet handle (fromIntegral size)
      
      -- Parse based on version
      return $ parseID3v2Tag version flags tagData metadata

-- | Parse synchsafe integer (ID3v2 uses 7 bits per byte for sizes)
parseSynchsafeInt :: BS.ByteString -> Word32
parseSynchsafeInt bs =
  case map fromIntegral $ BS.unpack $ BS.take 4 bs of
    [b1, b2, b3, b4] -> (b1 `shiftL` 21) .|. (b2 `shiftL` 14) .|. (b3 `shiftL` 7) .|. b4
    _ -> 0  -- Should not happen with BS.take 4, but handle gracefully

-- | Parse ID3v2 tag data
parseID3v2Tag :: Word8 -> Word8 -> BS.ByteString -> Metadata -> Metadata
parseID3v2Tag version _flags tagData metadata =
  let frames = parseID3v2Frames version (L.fromStrict tagData)
      tagMap = HM.fromList frames
      -- Try to find and parse APIC frame for album art info (metadata only, not image data)
      parsedAlbumArtInfo = findAndParseAPICInfo version (L.fromStrict tagData)
  in metadata
    { title = HM.lookup "TIT2" tagMap <|> HM.lookup "TT2" tagMap
    , artist = HM.lookup "TPE1" tagMap <|> HM.lookup "TP1" tagMap
    , album = HM.lookup "TALB" tagMap <|> HM.lookup "TAL" tagMap
    , albumArtist = HM.lookup "TPE2" tagMap <|> HM.lookup "TP2" tagMap
    , trackNumber = (HM.lookup "TRCK" tagMap <|> HM.lookup "TRK" tagMap) >>= parseTrackNumber
    , year = ((HM.lookup "TYER" tagMap <|> HM.lookup "TYE" tagMap) >>= readInt)
             <|> (HM.lookup "TDRC" tagMap >>= extractYearFromDate)
    , date = HM.lookup "TDRC" tagMap
    , genre = HM.lookup "TCON" tagMap <|> HM.lookup "TCO" tagMap
    , comment = HM.lookup "COMM" tagMap <|> HM.lookup "COM" tagMap <|> HM.lookup "TXXX:comment" tagMap
    , publisher = HM.lookup "TPUB" tagMap
    , recordLabel = HM.lookup "TXXX:LABEL" tagMap <|> HM.lookup "TPUB" tagMap
    , catalogNumber = HM.lookup "TXXX:CATALOGNUMBER" tagMap
    , barcode = HM.lookup "TXXX:BARCODE" tagMap
    , releaseCountry = HM.lookup "TXXX:MusicBrainz Album Release Country" tagMap
    , releaseStatus = HM.lookup "TXXX:MusicBrainz Album Status" tagMap
    , releaseType = HM.lookup "TXXX:MusicBrainz Album Type" tagMap
    , albumArtInfo = parsedAlbumArtInfo
    , musicBrainzIds = extractMusicBrainzIds tagMap
    , acoustidFingerprint = extractAcoustidFingerprint tagMap
    , acoustidId = extractAcoustidId tagMap
    , rawTags = tagMap  -- Populate rawTags with all parsed frames
    }
  where
    parseTrackNumber t = case T.split (== '/') t of
      (n:_) -> readInt n
      _ -> Nothing
    extractMusicBrainzIds tags = MusicBrainzIds
      { mbTrackId = HM.lookup "UFID:http://musicbrainz.org" tags
      , mbRecordingId = HM.lookup "TXXX:MusicBrainz Recording Id" tags
      , mbReleaseId = HM.lookup "TXXX:MusicBrainz Album Id" tags
      , mbReleaseGroupId = HM.lookup "TXXX:MusicBrainz Release Group Id" tags
      , mbArtistId = HM.lookup "TXXX:MusicBrainz Artist Id" tags
      , mbAlbumArtistId = HM.lookup "TXXX:MusicBrainz Album Artist Id" tags
      , mbWorkId = HM.lookup "TXXX:MusicBrainz Work Id" tags
      , mbDiscId = HM.lookup "TXXX:MusicBrainz Disc Id" tags
      }
    extractAcoustidFingerprint tags = 
      HM.lookup "TXXX:Acoustid Fingerprint" tags <|>
      HM.lookup "TXXX:ACOUSTID_FINGERPRINT" tags <|>
      HM.lookup "TXXX:acoustid_fingerprint" tags
    extractAcoustidId tags =
      HM.lookup "TXXX:Acoustid Id" tags <|>
      HM.lookup "TXXX:ACOUSTID_ID" tags <|>
      HM.lookup "TXXX:acoustid_id" tags

-- | Parse ID3v2 frames
parseID3v2Frames :: Word8 -> L.ByteString -> [(Text, Text)]
parseID3v2Frames version bs
  | version == 2 = parseID3v22Frames bs  -- 3-char frame IDs
  | version >= 3 = parseID3v23Frames version bs  -- 4-char frame IDs
  | otherwise = []

-- | Parse ID3v2.2 frames (3-character IDs)
parseID3v22Frames :: L.ByteString -> [(Text, Text)]
parseID3v22Frames bs
  | L.length bs < 6 = []
  | otherwise = 
      case runGetOrFail parseID3v22Frame bs of
        Left _ -> []
        Right (rest, _, Nothing) ->
          parseID3v22Frames rest  -- Skip and continue
        Right (rest, _, Just frame) ->
          frame : parseID3v22Frames rest

parseID3v22Frame :: Get (Maybe (Text, Text))
parseID3v22Frame = do
  frameId <- getByteString 3
  -- Check for padding
  if BS.all (== 0) frameId
    then fail "Padding reached"
    else do
      -- 24-bit size (big-endian)
      sizeByte1 <- getWord8
      sizeByte2 <- getWord8
      sizeByte3 <- getWord8
      let frameSize = ((fromIntegral sizeByte1 :: Word32) `shiftL` 16) .|.
                      ((fromIntegral sizeByte2 :: Word32) `shiftL` 8) .|.
                      (fromIntegral sizeByte3 :: Word32)
      
      frameData <- getByteString (fromIntegral frameSize)
      -- Skip PIC frames (ID3v2.2 version of APIC)
      if frameId == "PIC"
        then return Nothing
        else do
          let frameIdText = TE.decodeUtf8With TEE.lenientDecode frameId
          let frameValue = 
                if frameId == "COM"
                then parseCOMMFrame frameData
                else if frameId == "TXX"
                then snd $ parseTXXXFrame frameData  -- v2.2 doesn't have description prefix
                else if frameId == "ULT"
                then parseUSLTFrame frameData
                else parseFrameContent frameData
          return $ Just (frameIdText, frameValue)

-- | Parse ID3v2.3+ frames (4-character IDs)
parseID3v23Frames :: Word8 -> L.ByteString -> [(Text, Text)]
parseID3v23Frames version bs
  | L.length bs < 10 = []
  | otherwise =
      case runGetOrFail (parseID3v23Frame version) bs of
        Left _ -> []
        Right (rest, _, Nothing) ->
          -- Frame was skipped (e.g., APIC), continue with the rest
          parseID3v23Frames version rest
        Right (rest, _, Just frame) ->
          frame : parseID3v23Frames version rest

parseID3v23Frame :: Word8 -> Get (Maybe (Text, Text))
parseID3v23Frame version = do
  frameId <- getByteString 4
  -- Check for padding
  if BS.all (== 0) frameId
    then fail "Padding reached"
    else do
      -- Frame size (synchsafe for v2.4, normal for v2.3)
      frameSize <- if version >= 4
        then do
          b1 <- getWord8
          b2 <- getWord8
          b3 <- getWord8
          b4 <- getWord8
          return $ (fromIntegral b1 `shiftL` 21) .|.
                   (fromIntegral b2 `shiftL` 14) .|.
                   (fromIntegral b3 `shiftL` 7) .|.
                   fromIntegral b4
        else getWord32be
      
      _ <- getWord16be  -- frameFlags
      frameData <- getByteString (fromIntegral frameSize)
      
      -- Skip APIC frames in this text-only parser
      if frameId == "APIC"
        then return Nothing  -- Skip picture frames but continue parsing
        else do
          let frameIdText = TE.decodeUtf8With TEE.lenientDecode frameId
          let (finalId, frameValue) = 
                if frameId == "COMM" || frameId == "COM"
                then (frameIdText, parseCOMMFrame frameData)
                else if frameId == "TXXX"
                then parseTXXXFrame frameData
                else if frameId == "USLT"  -- Add support for lyrics
                then (frameIdText, parseUSLTFrame frameData)
                else (frameIdText, parseFrameContent frameData)
          return $ Just (finalId, frameValue)

-- | Parse frame content (simplified - handles text frames)
parseFrameContent :: BS.ByteString -> Text
parseFrameContent bs
  | BS.null bs = ""
  | otherwise = 
      let encoding = BS.head bs
          content = BS.tail bs
      in T.filter (/= '\0') $ case encoding of  -- Strip null terminators after decoding
        0 -> -- ISO-8859-1: treat as Latin-1
          TE.decodeLatin1 content
        1 -> -- UTF-16 with BOM
          decodeUtf16 content
        2 -> -- UTF-16BE without BOM
          TE.decodeUtf16BEWith TEE.lenientDecode content
        3 -> -- UTF-8
          TE.decodeUtf8With TEE.lenientDecode content
        _ -> -- Unknown, try UTF-8
          TE.decodeUtf8With TEE.lenientDecode content
  where
    decodeUtf16 bytes = 
      -- Check for BOM and decode accordingly
      if BS.length bytes >= 2
        then case (BS.index bytes 0, BS.index bytes 1) of
          (0xFF, 0xFE) -> TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 bytes)
          (0xFE, 0xFF) -> TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 bytes)
          _ -> TE.decodeUtf16LEWith TEE.lenientDecode bytes
        else ""
    _decodeUtf16BE = TE.decodeUtf16BEWith TEE.lenientDecode

-- | Parse COMM (comment) frame which has special structure
parseCOMMFrame :: BS.ByteString -> Text
parseCOMMFrame bs
  | BS.length bs < 5 = ""  -- Need at least encoding + language (3) + content
  | otherwise = 
      let encoding = BS.head bs
          rest = BS.drop 4 bs  -- Skip encoding + 3-byte language code
          -- Find the null terminator after the short description
          (_description, afterDesc) = case encoding of
            1 -> BS.breakSubstring "\0\0" rest  -- UTF-16 uses double null
            2 -> BS.breakSubstring "\0\0" rest  -- UTF-16BE uses double null
            _ -> BS.breakSubstring "\0" rest     -- ISO-8859-1 and UTF-8 use single null
          -- Skip the description and null terminator(s)
          content = case encoding of
            1 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            2 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            _ -> if BS.null afterDesc then BS.empty else BS.drop 1 afterDesc  -- Skip \0
          -- Decode the actual comment text
      in case encoding of
        0 -> TE.decodeLatin1 content  -- ISO-8859-1 (Latin-1)
        1 -> decodeUtf16 content  -- UTF-16 with BOM
        2 -> TE.decodeUtf16BEWith TEE.lenientDecode content  -- UTF-16BE
        3 -> TE.decodeUtf8With TEE.lenientDecode content  -- UTF-8
        _ -> TE.decodeUtf8With TEE.lenientDecode content
  where
    decodeUtf16 content = 
      if BS.length content >= 2
        then case (BS.index content 0, BS.index content 1) of
          (0xFF, 0xFE) -> TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 content)
          (0xFE, 0xFF) -> TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 content)
          _ -> TE.decodeUtf16LEWith TEE.lenientDecode content
        else ""

-- | Parse TXXX frame (user-defined text information frame)
-- Returns (frameId with description, value)
parseTXXXFrame :: BS.ByteString -> (Text, Text)
parseTXXXFrame bs
  | BS.null bs = ("TXXX", "")
  | otherwise = 
      let encoding = BS.head bs
          rest = BS.tail bs
          -- Find the null terminator after the description
          (descBytes, afterDesc) = case encoding of
            1 -> BS.breakSubstring "\0\0" rest  -- UTF-16 uses double null
            2 -> BS.breakSubstring "\0\0" rest  -- UTF-16BE uses double null
            _ -> BS.breakSubstring "\0" rest     -- ISO-8859-1 and UTF-8 use single null
          -- Skip the description and null terminator(s)
          valueBytes = case encoding of
            1 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            2 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            _ -> if BS.null afterDesc then BS.empty else BS.drop 1 afterDesc  -- Skip \0
          -- Decode both description and value
          description = T.filter (/= '\0') $ decodeByEncoding encoding descBytes
          value = T.filter (/= '\0') $ decodeByEncoding encoding valueBytes
          frameId = if T.null description then "TXXX" else "TXXX:" <> description
      in (frameId, value)
  where
    decodeByEncoding 0 bytes = TE.decodeLatin1 bytes  -- ISO-8859-1
    decodeByEncoding 1 bytes = decodeUtf16 bytes  -- UTF-16 with BOM
    decodeByEncoding 2 bytes = TE.decodeUtf16BEWith TEE.lenientDecode bytes  -- UTF-16BE
    decodeByEncoding 3 bytes = TE.decodeUtf8With TEE.lenientDecode bytes  -- UTF-8
    decodeByEncoding _ bytes = TE.decodeUtf8With TEE.lenientDecode bytes
    
    decodeUtf16 bytes = 
      if BS.length bytes >= 2
        then case (BS.index bytes 0, BS.index bytes 1) of
          (0xFF, 0xFE) -> TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 bytes)
          (0xFE, 0xFF) -> TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 bytes)
          _ -> TE.decodeUtf16LEWith TEE.lenientDecode bytes
        else ""

-- | Parse USLT frame (unsynchronized lyrics/text transcription)
parseUSLTFrame :: BS.ByteString -> Text
parseUSLTFrame bs
  | BS.length bs < 5 = ""  -- Need at least encoding + language (3) + content
  | otherwise = 
      let encoding = BS.head bs
          rest = BS.drop 4 bs  -- Skip encoding + 3-byte language code
          -- Find the null terminator after the content descriptor
          (_, afterDesc) = case encoding of
            1 -> BS.breakSubstring "\0\0" rest  -- UTF-16 uses double null
            2 -> BS.breakSubstring "\0\0" rest  -- UTF-16BE uses double null
            _ -> BS.breakSubstring "\0" rest     -- ISO-8859-1 and UTF-8 use single null
          -- Skip the descriptor and null terminator(s)
          lyricsBytes = case encoding of
            1 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            2 -> if BS.null afterDesc then BS.empty else BS.drop 2 afterDesc  -- Skip \0\0
            _ -> if BS.null afterDesc then BS.empty else BS.drop 1 afterDesc  -- Skip \0
          -- Decode lyrics
      in T.filter (/= '\0') $ decodeByEncoding encoding lyricsBytes
  where
    decodeByEncoding 0 bytes = TE.decodeLatin1 bytes
    decodeByEncoding 1 bytes = decodeUtf16 bytes 
    decodeByEncoding 2 bytes = TE.decodeUtf16BEWith TEE.lenientDecode bytes
    decodeByEncoding 3 bytes = TE.decodeUtf8With TEE.lenientDecode bytes
    decodeByEncoding _ bytes = TE.decodeUtf8With TEE.lenientDecode bytes
    
    decodeUtf16 bytes = 
      if BS.length bytes >= 2
        then case (BS.index bytes 0, BS.index bytes 1) of
          (0xFF, 0xFE) -> TE.decodeUtf16LEWith TEE.lenientDecode (BS.drop 2 bytes)
          (0xFE, 0xFF) -> TE.decodeUtf16BEWith TEE.lenientDecode (BS.drop 2 bytes)
          _ -> TE.decodeUtf16LEWith TEE.lenientDecode bytes
        else ""

-- | Parse ID3v1 tag from file handle if present
parseID3v1FromHandle :: Handle -> Metadata -> IO Metadata
parseID3v1FromHandle handle metadata = do
  fileSize <- hFileSize handle
  if fileSize > 128
    then do
      hSeek handle AbsoluteSeek (fileSize - 128)
      id3v1Data <- BS.hGet handle 128
      if BS.take 3 id3v1Data == "TAG"
        then return $ parseID3v1Tag id3v1Data metadata
        else return metadata
    else return metadata

-- | Parse ID3v1 tag
parseID3v1Tag :: BS.ByteString -> Metadata -> Metadata
parseID3v1Tag bs metadata = metadata
  { title = title metadata <|> parseID3v1Field bs 3 30
  , artist = artist metadata <|> parseID3v1Field bs 33 30
  , album = album metadata <|> parseID3v1Field bs 63 30
  , year = year metadata <|> (parseID3v1Field bs 93 4 >>= readInt)
  , comment = comment metadata <|> parseID3v1Field bs 97 28
  , trackNumber = trackNumber metadata <|>
      if BS.index bs 125 == 0 && BS.index bs 126 /= 0
      then Just (fromIntegral $ BS.index bs 126)
      else Nothing
  }
  where
    parseID3v1Field bytes offset len =
      let field = BS.take len (BS.drop offset bytes)
          cleaned = BS.takeWhile (/= 0) field
      in if BS.null cleaned
         then Nothing
         else Just $ TE.decodeUtf8With TEE.lenientDecode cleaned

-- | Parse MP3 audio properties from file handle
parseMP3AudioPropertiesFromHandle :: Handle -> IO AudioProperties
parseMP3AudioPropertiesFromHandle handle = do
  -- Skip ID3v2 tag if present
  hSeek handle AbsoluteSeek 0
  startPos <- skipID3v2 handle
  
  -- Get file size for duration calculation
  fileSize <- hFileSize handle
  
  -- Seek to where audio should start
  hSeek handle AbsoluteSeek startPos
  
  -- Search for MP3 frame sync within first 1MB (like mutagen)
  frameHeader <- findMP3FrameSync handle (1024 * 1024)
  
  case frameHeader of
    Just header -> do
      -- Parse basic frame info
      let props = parseMP3FrameHeader header
      -- Try to find VBR headers (Xing/Info or VBRI)
      currentPos <- hTell handle
      hSeek handle AbsoluteSeek (currentPos - 4)  -- Go back to frame start
      vbrProps <- parseVBRHeaders handle props
      
      -- If no duration calculated yet (CBR file), calculate from file size and bitrate
      let finalProps = case (duration vbrProps, bitrate vbrProps) of
            (Nothing, Just br) | br > 0 -> 
              let audioSize = fileSize - startPos  -- Exclude ID3 tags
                  durationSecs = (fromIntegral audioSize * 8) / (fromIntegral br * 1000) :: Double
                  durationMs = round (durationSecs * 1000) :: Int
              in vbrProps { duration = Just durationMs }
            _ -> vbrProps
      
      return finalProps
    Nothing -> return emptyAudioProperties

-- | Skip ID3v2 tag and return position after it
skipID3v2 :: Handle -> IO Integer
skipID3v2 handle = do
  header <- BS.hGet handle 10
  if BS.length header < 10 || BS.take 3 header /= "ID3"
    then return 0
    else do
      let size = parseSynchsafeInt (BS.drop 6 header)
      return $ 10 + fromIntegral size

-- | Find MP3 frame sync
findMP3FrameSync :: Handle -> Int -> IO (Maybe BS.ByteString)
findMP3FrameSync handle maxBytes = searchSync 0
  where
    searchSync bytesRead'
      | bytesRead' >= maxBytes = return Nothing
      | otherwise = do
          chunk <- BS.hGet handle (min 4096 (maxBytes - bytesRead'))
          if BS.null chunk
            then return Nothing
            else case findSync chunk of
              Just pos -> do
                hSeek handle RelativeSeek (fromIntegral pos - fromIntegral (BS.length chunk))
                header <- BS.hGet handle 4
                return $ Just header
              Nothing -> searchSync (bytesRead' + BS.length chunk)
    
    findSync bs = 
      let indices = [i | i <- [0..BS.length bs - 2]
                       , BS.index bs i == 0xFF
                       , (BS.index bs (i+1) .&. 0xE0) == 0xE0]
      in case indices of
        (i:_) -> Just i
        _ -> Nothing

-- | Parse MP3 frame header
parseMP3FrameHeader :: BS.ByteString -> AudioProperties
parseMP3FrameHeader header =
  let byte2 = BS.index header 1
      versionBits = (byte2 `shiftR` 3) .&. 0x03
      layerBits = (byte2 `shiftR` 1) .&. 0x03
      
      byte3 = BS.index header 2
      bitrateBits = (byte3 `shiftR` 4) .&. 0x0F
      sampleRateBits = (byte3 `shiftR` 2) .&. 0x03
      _paddingBit = (byte3 `shiftR` 1) .&. 0x01
      
      byte4 = BS.index header 3
      channelMode = (byte4 `shiftR` 6) .&. 0x03
      
      -- Determine MPEG version
      version = case versionBits of
        0 -> 2.5 :: Double  -- MPEG 2.5
        2 -> 2    -- MPEG 2
        3 -> 1    -- MPEG 1
        _ -> 1    -- Invalid, default to 1
      
      -- Determine layer
      layer = case layerBits of
        1 -> 3 :: Int -- Layer III
        2 -> 2  -- Layer II
        3 -> 1  -- Layer I
        _ -> 0  -- Invalid
      
      -- Look up sample rate
      sampleRate' = case (versionBits, sampleRateBits) of
        (0, 0) -> 11025  -- MPEG 2.5
        (0, 1) -> 12000
        (0, 2) -> 8000
        (2, 0) -> 22050  -- MPEG 2
        (2, 1) -> 24000
        (2, 2) -> 16000
        (3, 0) -> 44100  -- MPEG 1
        (3, 1) -> 48000
        (3, 2) -> 32000
        _ -> 44100
      
      -- Look up bitrate (in kbps)
      -- This is for Layer III (MP3)
      bitrate' = if layer == 3 && bitrateBits /= 0 && bitrateBits /= 15
        then case version of
          1 -> [0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0] !! fromIntegral bitrateBits
          _ -> [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0] !! fromIntegral bitrateBits
        else 0  -- Free bitrate or invalid
      
      channels' = if channelMode == 3 then 1 else 2
      
  in emptyAudioProperties
    { sampleRate = Just sampleRate'
    , channels = Just channels'
    , bitrate = if bitrate' > 0 then Just bitrate' else Nothing
    }

-- | Parse VBR headers (Xing/Info or VBRI)
parseVBRHeaders :: Handle -> AudioProperties -> IO AudioProperties
parseVBRHeaders handle props = do
  -- Read frame header + side info
  frameData <- BS.hGet handle 200  -- Should be enough for frame header + VBR headers
  
  if BS.length frameData < 40
    then return props
    else do
      -- Check for Xing/Info header (usually at offset 36 for stereo, 21 for mono)
      let numChannels = fromMaybe 2 (channels props)
          xingOffset = if numChannels == 1 then 21 else 36
      
      if BS.length frameData > xingOffset + 12
        then do
          let xingHeader = BS.take 4 $ BS.drop xingOffset frameData
          if xingHeader `elem` ["Xing", "Info"]
            then parseXingHeader (BS.drop (xingOffset + 4) frameData) props handle
            else
              -- Check for VBRI header (always at offset 36)
              if BS.length frameData > 36 + 26
                then do
                  let vbriHeader = BS.take 4 $ BS.drop 36 frameData
                  if vbriHeader == "VBRI"
                    then parseVBRIHeader (BS.drop 40 frameData) props handle
                    else return props
                else return props
        else return props

-- | Parse Xing/Info VBR header
parseXingHeader :: BS.ByteString -> AudioProperties -> Handle -> IO AudioProperties
parseXingHeader bs props _handle = do
  if BS.length bs < 8
    then return props
    else do
      let flags = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          hasFrames = (flags .&. 0x01) /= 0
          hasBytes = (flags .&. 0x02) /= 0
          
      -- Parse frame count and byte count if present
      let offset1 = 4
          (frameCount, offset2) = if hasFrames
            then (Just $ runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop offset1 bs), offset1 + 4)
            else (Nothing, offset1)
          (byteCount, _) = if hasBytes
            then (Just $ runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop offset2 bs), offset2 + 4)
            else (Nothing, offset2)
      
      -- Calculate average bitrate and duration for VBR
      case (frameCount, byteCount, sampleRate props) of
        (Just frames, Just bytes, Just sr) -> do
          -- MP3 frame is 1152 samples for Layer III
          let durationSecs = (fromIntegral frames * 1152) / fromIntegral sr :: Double
              durationMs = round (durationSecs * 1000) :: Int  -- Convert to milliseconds
              avgBitrate = if durationSecs > 0
                then round $ (fromIntegral bytes * 8) / durationSecs / 1000
                else 0
          return props { bitrate = if avgBitrate > 0 then Just avgBitrate else bitrate props
                       , duration = if durationMs > 0 then Just durationMs else duration props }
        _ -> return props

-- | Parse VBRI VBR header  
parseVBRIHeader :: BS.ByteString -> AudioProperties -> Handle -> IO AudioProperties
parseVBRIHeader bs props _handle = do
  if BS.length bs < 22
    then return props
    else do
      -- fileSize <- hFileSize _handle
      let _version = runGet getWord16be (L.fromStrict $ BS.take 2 bs)
          -- Skip delay and quality (2 + 2 bytes)
          bytes = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 6 bs)
          frames = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 10 bs)
      
      -- Calculate average bitrate and duration
      case sampleRate props of
        Just sr -> do
          let durationSecs = (fromIntegral frames * 1152) / fromIntegral sr :: Double
              durationMs = round (durationSecs * 1000) :: Int  -- Convert to milliseconds
              avgBitrate = if durationSecs > 0
                then round $ (fromIntegral bytes * 8) / durationSecs / 1000
                else 0
          return props { bitrate = if avgBitrate > 0 then Just avgBitrate else bitrate props
                       , duration = if durationMs > 0 then Just durationMs else duration props }
        _ -> return props

-- | Find and parse APIC frame info in ID3v2 tag data (metadata only, no image data)
findAndParseAPICInfo :: Word8 -> L.ByteString -> Maybe AlbumArtInfo
findAndParseAPICInfo version bs
  | version < 3 = Nothing  -- APIC only in ID3v2.3+
  | otherwise = go bs
  where
    go bytes
      | L.length bytes < 10 = Nothing
      | otherwise =
          case runGetOrFail (findAPICFrame version) bytes of
            Left _ -> Nothing
            Right (_, _, Just pic) -> Just pic
            Right (rest, consumed, Nothing)
              | consumed == 0 -> Nothing  -- No bytes consumed, stop to avoid infinite loop
              | otherwise -> go rest

    findAPICFrame :: Word8 -> Get (Maybe AlbumArtInfo)
    findAPICFrame _version = do
      frameId <- lookAhead $ getByteString 4
      if frameId == "APIC"
        then do
          -- Parse the APIC frame
          _ <- getByteString 4  -- Consume frame ID
          frameSize <- if version >= 4
            then do
              b1 <- getWord8
              b2 <- getWord8
              b3 <- getWord8
              b4 <- getWord8
              return $ (fromIntegral b1 `shiftL` 21) .|.
                       (fromIntegral b2 `shiftL` 14) .|.
                       (fromIntegral b3 `shiftL` 7) .|.
                       fromIntegral b4
            else getWord32be
          _ <- getWord16be  -- Skip flags
          frameData <- getByteString (fromIntegral frameSize)
          return $ parseAPICFrameInfo frameData
        else if BS.all (== 0) frameId
          then return Nothing  -- Padding reached
          else do
            -- Skip this frame
            _ <- getByteString 4
            frameSize <- if version >= 4
              then do
                b1 <- getWord8
                b2 <- getWord8
                b3 <- getWord8
                b4 <- getWord8
                return $ (fromIntegral b1 `shiftL` 21) .|.
                         (fromIntegral b2 `shiftL` 14) .|.
                         (fromIntegral b3 `shiftL` 7) .|.
                         fromIntegral b4
              else getWord32be
            _ <- getWord16be
            skip (fromIntegral frameSize)
            return Nothing

-- | Parse APIC (Attached Picture) frame info (metadata only, not image data for performance)
parseAPICFrameInfo :: BS.ByteString -> Maybe AlbumArtInfo
parseAPICFrameInfo bs =
  if BS.null bs
    then Nothing
    else
      let _encoding = BS.head bs
          rest = BS.tail bs
      in case BS.breakSubstring "\0" rest of
        (mimeType, afterMime) ->
          if BS.null afterMime
            then Nothing
            else
              let rest2 = BS.drop 1 afterMime  -- Skip null terminator
                  pictureType = if BS.null rest2 then 0 else BS.head rest2
                  rest3 = if BS.null rest2 then BS.empty else BS.tail rest2
              in case BS.breakSubstring "\0" rest3 of
                (description, afterDesc) ->
                  if BS.null afterDesc
                    then Nothing
                    else
                      -- Don't read image data, just calculate its size
                      let imageDataSize = BS.length (BS.drop 1 afterDesc)
                      in Just $ AlbumArtInfo
                        { albumArtInfoMimeType = TE.decodeUtf8With TEE.lenientDecode mimeType
                        , albumArtInfoPictureType = pictureType
                        , albumArtInfoDescription = TE.decodeUtf8With TEE.lenientDecode description
                        , albumArtInfoSizeBytes = imageDataSize
                        }

-- | Load album art from MP3 file (full binary data for writing)
loadAlbumArtMP3 :: OsPath -> Parser (Maybe AlbumArt)
loadAlbumArtMP3 filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Parse ID3v2 tag at beginning if present
    hSeek handle AbsoluteSeek 0
    header <- BS.hGet handle 10

    if BS.length header < 10 || BS.take 3 header /= id3v2Signature
      then return $ Right Nothing  -- No ID3v2 tag
      else do
        -- Parse header to get tag size
        let version = BS.index header 3
            size = parseSynchsafeInt (BS.drop 6 header)

        -- Read only the ID3v2 tag data
        tagData <- BS.hGet handle (fromIntegral size)

        -- Find and parse APIC frame with full data
        return $ Right $ findAndParseAPICFull version (L.fromStrict tagData)

  case result of
    Left err -> throwError err
    Right maybeArt -> return maybeArt
  where
    findAndParseAPICFull :: Word8 -> L.ByteString -> Maybe AlbumArt
    findAndParseAPICFull version bs
      | version < 3 = Nothing  -- APIC only in ID3v2.3+
      | otherwise = go bs
      where
        go bytes
          | L.length bytes < 10 = Nothing
          | otherwise =
              case runGetOrFail (findAPICFrame version) bytes of
                Left _ -> Nothing
                Right (_, _, Just pic) -> Just pic
                Right (rest, consumed, Nothing)
                  | consumed == 0 -> Nothing
                  | otherwise -> go rest

        findAPICFrame :: Word8 -> Get (Maybe AlbumArt)
        findAPICFrame _version = do
          frameId <- lookAhead $ getByteString 4
          if frameId == "APIC"
            then do
              _ <- getByteString 4  -- Consume frame ID
              frameSize <- if version >= 4
                then do
                  b1 <- getWord8
                  b2 <- getWord8
                  b3 <- getWord8
                  b4 <- getWord8
                  return $ (fromIntegral b1 `shiftL` 21) .|.
                           (fromIntegral b2 `shiftL` 14) .|.
                           (fromIntegral b3 `shiftL` 7) .|.
                           fromIntegral b4
                else getWord32be
              _ <- getWord16be  -- Skip flags
              frameData <- getByteString (fromIntegral frameSize)
              return $ parseAPICFrameFull frameData
            else if BS.all (== 0) frameId
              then return Nothing
              else do
                _ <- getByteString 4
                frameSize <- if version >= 4
                  then do
                    b1 <- getWord8
                    b2 <- getWord8
                    b3 <- getWord8
                    b4 <- getWord8
                    return $ (fromIntegral b1 `shiftL` 21) .|.
                             (fromIntegral b2 `shiftL` 14) .|.
                             (fromIntegral b3 `shiftL` 7) .|.
                             fromIntegral b4
                  else getWord32be
                _ <- getWord16be
                skip (fromIntegral frameSize)
                return Nothing

    parseAPICFrameFull :: BS.ByteString -> Maybe AlbumArt
    parseAPICFrameFull bs =
      if BS.null bs
        then Nothing
        else
          let _encoding = BS.head bs
              rest = BS.tail bs
          in case BS.breakSubstring "\0" rest of
            (mimeType, afterMime) ->
              if BS.null afterMime
                then Nothing
                else
                  let rest2 = BS.drop 1 afterMime
                      pictureType = if BS.null rest2 then 0 else BS.head rest2
                      rest3 = if BS.null rest2 then BS.empty else BS.tail rest2
                  in case BS.breakSubstring "\0" rest3 of
                    (description, afterDesc) ->
                      if BS.null afterDesc
                        then Nothing
                        else
                          let imageData = BS.drop 1 afterDesc
                          in Just $ AlbumArt
                            { albumArtMimeType = TE.decodeUtf8With TEE.lenientDecode mimeType
                            , albumArtPictureType = pictureType
                            , albumArtDescription = TE.decodeUtf8With TEE.lenientDecode description
                            , albumArtData = imageData
                            }

-- | Extract year from TDRC date field (YYYY-MM-DD or just YYYY)
extractYearFromDate :: T.Text -> Maybe Int
extractYearFromDate dateText =
  let yearStr = T.takeWhile (/= '-') dateText
  in readInt yearStr
