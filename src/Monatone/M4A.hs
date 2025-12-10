{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Monatone.M4A
  ( parseM4A
  , loadAlbumArtM4A
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Maybe (listToMaybe)
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

-- | MP4 atom structure
data Atom = Atom
  { atomName :: BS.ByteString
  , atomSize :: Word64
  , atomOffset :: Integer
  , atomChildren :: Maybe [Atom]
  , atomDataOffset :: Integer
  } deriving (Show, Eq)

-- | Container atoms that have children
containerAtoms :: [BS.ByteString]
containerAtoms = ["moov", "udta", "trak", "mdia", "meta", "ilst", "stbl", "minf", "moof", "traf", "stsd"]

-- | Parse M4A file
parseM4A :: OsPath -> Parser Metadata
parseM4A filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    -- Parse atom structure
    atoms <- parseAtoms handle

    -- Extract metadata from ilst atom
    metadata <- extractMetadata handle atoms (emptyMetadata M4A)

    -- Parse audio properties
    audioProps <- extractAudioProperties handle atoms

    return $ Right $ metadata { audioProperties = audioProps }

  case result of
    Left err -> throwError err
    Right m -> return m

-- | Parse all top-level atoms
parseAtoms :: Handle -> IO [Atom]
parseAtoms handle = do
  fileSize <- hFileSize handle
  hSeek handle AbsoluteSeek 0
  parseAtomsUntil handle fileSize

-- | Parse atoms until we reach the end position
parseAtomsUntil :: Handle -> Integer -> IO [Atom]
parseAtomsUntil handle endPos = do
  pos <- hTell handle
  -- putStrLn $ "parseAtomsUntil: pos=" ++ show pos ++ ", endPos=" ++ show endPos
  if pos + 8 > endPos
    then return []
    else do
      maybeAtom <- parseAtom handle 0
      case maybeAtom of
        Nothing -> return []
        Just atom -> do
          -- putStrLn $ "Parsed atom: " ++ show (atomName atom) ++ " at " ++ show (atomOffset atom)
          rest <- parseAtomsUntil handle endPos
          return (atom : rest)

-- | Parse a single atom
parseAtom :: Handle -> Int -> IO (Maybe Atom)
parseAtom handle _level = do
  offset <- hTell handle
  headerData <- BS.hGet handle 8

  if BS.length headerData < 8
    then return Nothing
    else do
      let (size32, name) = runGet ((,) <$> getWord32be <*> getByteString 4) (L.fromStrict headerData)

      -- Handle 64-bit size
      (actualSize, dataOffset) <- if size32 == 1
        then do
          size64Data <- BS.hGet handle 8
          let size64 = runGet getWord64be (L.fromStrict size64Data)
          return (size64, offset + 16)
        else if size32 == 0
          then do
            -- Size extends to end of file
            fileSize <- hFileSize handle
            return (fromIntegral (fileSize - offset), offset + 8)
        else return (fromIntegral size32, offset + 8)

      -- Check if this is a container atom
      -- Note: We'll determine if we need children during parsing
      let isContainer = name `elem` containerAtoms

      children <- if isContainer
        then do
          -- For meta atom, skip 4 bytes (version/flags)
          let skipBytes :: Integer
              skipBytes = if name == "meta" then 4 else 0
          hSeek handle AbsoluteSeek (dataOffset + skipBytes)

          -- Parse children
          let endPos = offset + fromIntegral actualSize
          childList <- parseAtomsUntil handle endPos
          -- Debug moov children
          -- when (name == "moov") $ putStrLn $ "moov children: " ++ show (map atomName childList)

          -- IMPORTANT: Seek to end of this atom so next sibling can be parsed
          hSeek handle AbsoluteSeek (offset + fromIntegral actualSize)
          return childList
        else do
          -- Seek to end of this atom
          hSeek handle AbsoluteSeek (offset + fromIntegral actualSize)
          return []

      let atom = Atom
            { atomName = name
            , atomSize = actualSize
            , atomOffset = offset
            , atomChildren = if isContainer then Just children else Nothing
            , atomDataOffset = dataOffset + if name == "meta" then 4 else 0
            }

      return $ Just atom

-- | Find atom by path (e.g., ["moov", "udta", "meta", "ilst"])
findAtomPath :: [Atom] -> [BS.ByteString] -> Maybe Atom
findAtomPath _ [] = Nothing
findAtomPath atoms [name] = listToMaybe $ filter (\a -> atomName a == name) atoms
findAtomPath atoms (name:rest) = do
  atom <- listToMaybe $ filter (\a -> atomName a == name) atoms
  children <- atomChildren atom
  findAtomPath children rest

-- | Extract metadata from ilst atom
extractMetadata :: Handle -> [Atom] -> Metadata -> IO Metadata
extractMetadata handle atoms metadata = do
  -- Debug: check what atoms we have
  -- putStrLn $ "Top level atoms: " ++ show (map atomName atoms)
  case findAtomPath atoms ["moov", "udta", "meta", "ilst"] of
    Nothing -> do
      -- putStrLn "ilst atom not found!"
      return metadata
    Just ilstAtom -> do
      -- putStrLn $ "Found ilst, children: " ++ show (fmap (map atomName) (atomChildren ilstAtom))
      case atomChildren ilstAtom of
        Nothing -> return metadata
        Just children -> do
          -- Parse each tag atom
          tags <- mapM (parseTagAtom handle) children
          let tagMap = HM.fromList $ concat tags

          -- Parse album art info separately
          artInfo <- extractAlbumArtInfo handle children

          return $ (applyTags tagMap metadata) { albumArtInfo = artInfo }

-- | Parse a single tag atom from ilst
parseTagAtom :: Handle -> Atom -> IO [(Text, Text)]
parseTagAtom handle atom = do
  hSeek handle AbsoluteSeek (atomDataOffset atom)
  let dataSize = fromIntegral (atomSize atom) - fromIntegral (atomDataOffset atom - atomOffset atom)

  if dataSize <= 0
    then return []
    else do
      atomData <- BS.hGet handle dataSize
      return $ parseTagData (atomName atom) atomData

-- | Parse tag data - handles special atoms differently
parseTagData :: BS.ByteString -> BS.ByteString -> [(Text, Text)]
parseTagData "trkn" bs = parseTrackDiskAtom "trkn" bs
parseTagData "disk" bs = parseTrackDiskAtom "disk" bs
parseTagData "covr" _bs = []  -- Skip cover art in text parsing
parseTagData "----" bs = parseFreeformAtom bs  -- Freeform/custom tags
parseTagData name bs = parseDataAtoms name bs

-- | Parse track/disk number atoms (special binary format)
parseTrackDiskAtom :: BS.ByteString -> BS.ByteString -> [(Text, Text)]
parseTrackDiskAtom tagName bs
  | BS.length bs < 16 = []
  | otherwise =
      let size = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          dataName = BS.take 4 $ BS.drop 4 bs
      in if dataName /= "data" || size < 16
        then []
        else
          -- Data atom structure: [size:4][name:4][version/flags:4][data...]
          -- The flags contain the data type. Content starts at offset 16.
          let dataContent = BS.take (fromIntegral size - 16) $ BS.drop 16 bs
              -- Atom names use Latin-1 encoding
              key = TE.decodeLatin1 tagName
          in if BS.length dataContent >= 6
            then
              let current = runGet getWord16be (L.fromStrict $ BS.drop 2 dataContent)
                  total = runGet getWord16be (L.fromStrict $ BS.drop 4 dataContent)
                  currentText = T.pack $ show current
                  totalText = T.pack $ show total
              in [(key <> ":current", currentText), (key <> ":total", totalText)]
            else []

-- | Parse data atoms within a tag atom
parseDataAtoms :: BS.ByteString -> BS.ByteString -> [(Text, Text)]
parseDataAtoms tagName bs
  | BS.length bs < 16 = []
  | otherwise =
      let size = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          dataName = BS.take 4 $ BS.drop 4 bs
      in if dataName /= "data" || size < 16
        then []
        else
          -- Data atom structure: [size:4][name:4][version/flags:4][data...]
          -- The flags (lower 3 bytes of version/flags) contain the data type
          -- Offsets: 0=size, 4=name, 8=version/flags, 12=data
          let versionFlags = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 8 bs)
              dataType = versionFlags  -- The whole field is used as type (version is always 0)
              dataContent = BS.take (fromIntegral size - 16) $ BS.drop 16 bs
              rest = BS.drop (fromIntegral size) bs

              -- Decode based on type
              value = decodeDataValue dataType dataContent
              -- Atom names use Latin-1 encoding (©nam is 0xA9 0x6E 0x61 0x6D)
              key = TE.decodeLatin1 tagName

              current = if not (T.null value) then [(key, value)] else []
              next = if BS.length rest >= 16 then parseDataAtoms tagName rest else []
          in current ++ next

-- | Decode data value based on type flags
decodeDataValue :: Word32 -> BS.ByteString -> Text
decodeDataValue flags bs
  | flags == 1 = TE.decodeUtf8With TEE.lenientDecode bs  -- UTF-8
  | flags == 2 = TE.decodeUtf16BEWith TEE.lenientDecode bs  -- UTF-16BE
  | flags == 13 || flags == 14 = ""  -- JPEG/PNG (skip for text parsing)
  | flags == 21 = decodeInteger bs  -- Integer
  | flags == 0 = decodeInteger bs  -- Implicit (often integer)
  | otherwise = TE.decodeUtf8With TEE.lenientDecode bs

-- | Decode integer from bytes
decodeInteger :: BS.ByteString -> Text
decodeInteger bs
  | BS.length bs == 1 = T.pack $ show $ BS.index bs 0
  | BS.length bs == 2 = T.pack $ show $ runGet getWord16be (L.fromStrict bs)
  | BS.length bs == 4 = T.pack $ show $ runGet getWord32be (L.fromStrict bs)
  | BS.length bs == 8 = T.pack $ show $ runGet getWord64be (L.fromStrict bs)
  | otherwise = ""

-- | Parse freeform (----) atoms
-- Structure: [mean atom][name atom][data atom(s)]
parseFreeformAtom :: BS.ByteString -> [(Text, Text)]
parseFreeformAtom bs
  | BS.length bs < 20 = []  -- Need at least mean header
  | otherwise =
      let meanSize = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          meanName = BS.take 4 $ BS.drop 4 bs
      in if meanName /= "mean" || meanSize < 12
        then []
        else
          let meanDataSize = fromIntegral meanSize - 12
              meanData = BS.take meanDataSize $ BS.drop 12 bs
              afterMean = BS.drop (fromIntegral meanSize) bs

              -- Parse name atom
              nameSize = if BS.length afterMean >= 4
                        then runGet getWord32be (L.fromStrict $ BS.take 4 afterMean)
                        else 0
              nameAtomName = if BS.length afterMean >= 8
                            then BS.take 4 $ BS.drop 4 afterMean
                            else ""
          in if nameAtomName /= "name" || nameSize < 12
            then []
            else
              let nameDataSize = fromIntegral nameSize - 12
                  nameData = BS.take nameDataSize $ BS.drop 12 afterMean
                  afterName = BS.drop (fromIntegral nameSize) afterMean

                  -- Build key as "----:mean:name"
                  meanText = TE.decodeUtf8With TEE.lenientDecode meanData
                  nameText = TE.decodeUtf8With TEE.lenientDecode nameData
                  key = "----:" <> meanText <> ":" <> nameText

                  -- Parse data atom(s) - reuse parseDataAtoms logic
                  -- The remaining bytes should be data atom(s)
              in parseDataAtoms (TE.encodeUtf8 key) afterName

-- | Apply parsed tags to metadata
applyTags :: HM.HashMap Text Text -> Metadata -> Metadata
applyTags tags metadata = metadata
  { title = HM.lookup "\169nam" tags
  , artist = HM.lookup "\169ART" tags
  , album = HM.lookup "\169alb" tags
  , albumArtist = HM.lookup "aART" tags
  , trackNumber = HM.lookup "trkn:current" tags >>= readInt
  , totalTracks = HM.lookup "trkn:total" tags >>= readInt
  , discNumber = HM.lookup "disk:current" tags >>= readInt
  , totalDiscs = HM.lookup "disk:total" tags >>= readInt
  , date = HM.lookup "\169day" tags
  , year = HM.lookup "\169day" tags >>= extractYear
  , genre = HM.lookup "\169gen" tags
  , comment = HM.lookup "\169cmt" tags
  , publisher = HM.lookup "\169pub" tags
  , releaseCountry = lookupFreeform "MusicBrainz Album Release Country" tags
  , recordLabel = lookupFreeform "LABEL" tags
  , catalogNumber = lookupFreeform "CATALOGNUMBER" tags
  , barcode = lookupFreeform "BARCODE" tags
  , musicBrainzIds = extractMusicBrainzIds tags
  , acoustidFingerprint = lookupFreeform "Acoustid Fingerprint" tags
  , acoustidId = lookupFreeform "Acoustid Id" tags
  , rawTags = tags
  }
  where
    extractYear dateText =
      let yearStr = T.takeWhile (/= '-') dateText
      in readInt yearStr

    -- Helper to look up freeform tags with common mean prefix
    lookupFreeform :: Text -> HM.HashMap Text Text -> Maybe Text
    lookupFreeform name tagMap =
      HM.lookup ("----:com.apple.iTunes:" <> name) tagMap

    extractMusicBrainzIds tagMap = MusicBrainzIds
      { mbTrackId = lookupFreeform "MusicBrainz Release Track Id" tagMap
      , mbRecordingId = lookupFreeform "MusicBrainz Track Id" tagMap
      , mbReleaseId = lookupFreeform "MusicBrainz Album Id" tagMap
      , mbReleaseGroupId = lookupFreeform "MusicBrainz Release Group Id" tagMap
      , mbArtistId = lookupFreeform "MusicBrainz Artist Id" tagMap
      , mbAlbumArtistId = lookupFreeform "MusicBrainz Album Artist Id" tagMap
      , mbWorkId = lookupFreeform "MusicBrainz Work Id" tagMap
      , mbDiscId = lookupFreeform "MusicBrainz Disc Id" tagMap
      }

-- | Extract album art info from ilst children
extractAlbumArtInfo :: Handle -> [Atom] -> IO (Maybe AlbumArtInfo)
extractAlbumArtInfo handle children = do
  case listToMaybe $ filter (\a -> atomName a == "covr") children of
    Nothing -> return Nothing
    Just covrAtom -> do
      hSeek handle AbsoluteSeek (atomDataOffset covrAtom)
      let dataSize = fromIntegral (atomSize covrAtom) - fromIntegral (atomDataOffset covrAtom - atomOffset covrAtom)
      if dataSize <= 0
        then return Nothing
        else do
          atomData <- BS.hGet handle dataSize
          return $ parseAlbumArtInfo atomData

-- | Parse album art info (lightweight, no image data)
parseAlbumArtInfo :: BS.ByteString -> Maybe AlbumArtInfo
parseAlbumArtInfo bs
  | BS.length bs < 16 = Nothing
  | otherwise =
      let size = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          dataName = BS.take 4 $ BS.drop 4 bs
      in if dataName /= "data" || size < 16
        then Nothing
        else
          let flags = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 8 bs)
              imageDataSize = fromIntegral size - 16
              mimeType = case flags of
                13 -> "image/jpeg"  -- JPEG
                14 -> "image/png"   -- PNG
                27 -> "image/bmp"   -- BMP
                _ -> "image/unknown"
          in Just $ AlbumArtInfo
            { albumArtInfoMimeType = mimeType
            , albumArtInfoPictureType = 3  -- Front cover (iTunes default)
            , albumArtInfoDescription = ""
            , albumArtInfoSizeBytes = imageDataSize
            }

-- | Extract audio properties
extractAudioProperties :: Handle -> [Atom] -> IO AudioProperties
extractAudioProperties handle atoms = do
  -- Get duration from mvhd atom
  fileDuration <- extractDuration handle atoms

  -- Find the first audio track
  case findFirstAudioTrack atoms of
    Nothing -> return emptyAudioProperties { duration = fileDuration }
    Just trak -> do
      props <- parseAudioTrack handle trak
      return props { duration = fileDuration <|> Monatone.Metadata.duration props }

-- | Extract duration from mvhd atom
extractDuration :: Handle -> [Atom] -> IO (Maybe Int)
extractDuration handle atoms = do
  case findAtomPath atoms ["moov", "mvhd"] of
    Nothing -> return Nothing
    Just mvhdAtom -> do
      hSeek handle AbsoluteSeek (atomDataOffset mvhdAtom)
      mvhdData <- BS.hGet handle 32
      if BS.length mvhdData < 20
        then return Nothing
        else do
          let version = BS.index mvhdData 0
          if version == 0
            then do
              -- Version 0: 32-bit values
              let timescale = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 12 mvhdData)
                  durationValue = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 16 mvhdData)
                  durationMs = if timescale > 0
                    then Just $ round $ (fromIntegral durationValue / fromIntegral timescale :: Double) * 1000
                    else Nothing
              return durationMs
            else if version == 1
              then do
                -- Version 1: 64-bit values
                hSeek handle AbsoluteSeek (atomDataOffset mvhdAtom)
                mvhdDataLong <- BS.hGet handle 44
                if BS.length mvhdDataLong < 36
                  then return Nothing
                  else do
                    let timescale = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 20 mvhdDataLong)
                        durationValue = runGet getWord64be (L.fromStrict $ BS.take 8 $ BS.drop 24 mvhdDataLong)
                        durationMs = if timescale > 0
                          then Just $ round $ (fromIntegral durationValue / fromIntegral timescale :: Double) * 1000
                          else Nothing
                    return durationMs
              else return Nothing

-- | Find first audio track
findFirstAudioTrack :: [Atom] -> Maybe Atom
findFirstAudioTrack atoms = do
  moov <- findAtomPath atoms ["moov"]
  children <- atomChildren moov
  listToMaybe $ filter isAudioTrack children
  where
    isAudioTrack atom = atomName atom == "trak"

-- | Parse audio track properties
parseAudioTrack :: Handle -> Atom -> IO AudioProperties
parseAudioTrack handle trak = do
  -- Find sample description
  case atomChildren trak >>= \c -> findAtomPath c ["mdia", "minf", "stbl", "stsd"] of
    Nothing -> return emptyAudioProperties
    Just stsdAtom -> do
      -- Parse stsd atom
      hSeek handle AbsoluteSeek (atomDataOffset stsdAtom)
      stsdHeader <- BS.hGet handle 8
      if BS.length stsdHeader < 8
        then return emptyAudioProperties
        else do
          -- Skip version/flags (4 bytes) and entry count (4 bytes)
          -- Read first sample entry
          sampleEntry <- parseAtom handle 0
          case sampleEntry of
            Nothing -> return emptyAudioProperties
            Just entry -> parseSampleEntry handle entry

-- | Parse sample entry (mp4a, alac, etc.)
parseSampleEntry :: Handle -> Atom -> IO AudioProperties
parseSampleEntry handle entry = do
  hSeek handle AbsoluteSeek (atomDataOffset entry)
  entryData <- BS.hGet handle 28  -- AudioSampleEntry header

  if BS.length entryData < 28
    then return emptyAudioProperties
    else do
      let entryChannels = runGet getWord16be (L.fromStrict $ BS.take 2 $ BS.drop 16 entryData)
          entrySampleSize = runGet getWord16be (L.fromStrict $ BS.take 2 $ BS.drop 18 entryData)
          entrySampleRate = (runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 24 entryData)) `div` 65536

          codec = atomName entry

      -- Parse extension atoms for more details
      case atomChildren entry of
        Nothing -> return $ emptyAudioProperties
          { channels = Just $ fromIntegral entryChannels
          , bitsPerSample = Just $ fromIntegral entrySampleSize
          , sampleRate = Just $ fromIntegral entrySampleRate
          }
        Just exts -> do
          -- Look for esds (AAC) or alac atoms
          let esdsAtom = listToMaybe $ filter (\a -> atomName a == "esds") exts
          let alacAtom = listToMaybe $ filter (\a -> atomName a == "alac") exts

          case (codec, esdsAtom, alacAtom) of
            ("mp4a", Just esds, _) -> parseEsdsAtom handle esds entryChannels entrySampleSize entrySampleRate
            ("alac", _, Just alac) -> parseAlacAtom handle alac
            _ -> return $ emptyAudioProperties
              { channels = Just $ fromIntegral entryChannels
              , bitsPerSample = Just $ fromIntegral entrySampleSize
              , sampleRate = Just $ fromIntegral entrySampleRate
              }

-- | Parse ESDS atom for AAC info
parseEsdsAtom :: Handle -> Atom -> Word16 -> Word16 -> Word32 -> IO AudioProperties
parseEsdsAtom handle esds chans sampSize sampRate = do
  hSeek handle AbsoluteSeek (atomDataOffset esds)
  _esdsData <- BS.hGet handle 64  -- Should be enough

  -- For now, return basic info
  -- Full ESDS parsing is complex, would need to parse descriptors
  return emptyAudioProperties
    { channels = Just $ fromIntegral chans
    , bitsPerSample = Just $ fromIntegral sampSize
    , sampleRate = Just $ fromIntegral sampRate
    }

-- | Parse ALAC atom for Apple Lossless info
parseAlacAtom :: Handle -> Atom -> IO AudioProperties
parseAlacAtom handle alac = do
  hSeek handle AbsoluteSeek (atomDataOffset alac)
  alacData <- BS.hGet handle 36

  if BS.length alacData < 28
    then return emptyAudioProperties
    else do
      -- Skip version/flags (4 bytes) + frameLength (4 bytes) + compatibleVersion (1 byte)
      let alacSampleSize = BS.index alacData 9
          alacChannels = BS.index alacData 13
          alacSampleRate = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 20 alacData)

      return emptyAudioProperties
        { channels = Just $ fromIntegral alacChannels
        , bitsPerSample = Just $ fromIntegral alacSampleSize
        , sampleRate = Just $ fromIntegral alacSampleRate
        }

-- | Load album art from M4A file (full binary data for writing)
loadAlbumArtM4A :: OsPath -> Parser (Maybe AlbumArt)
loadAlbumArtM4A filePath = do
  result <- liftIO $ withBinaryFile filePath ReadMode $ \handle -> do
    atoms <- parseAtoms handle

    case findAtomPath atoms ["moov", "udta", "meta", "ilst"] of
      Nothing -> return $ Right Nothing
      Just ilstAtom -> do
        case atomChildren ilstAtom of
          Nothing -> return $ Right Nothing
          Just children -> do
            case listToMaybe $ filter (\a -> atomName a == "covr") children of
              Nothing -> return $ Right Nothing
              Just covrAtom -> do
                hSeek handle AbsoluteSeek (atomDataOffset covrAtom)
                let dataSize = fromIntegral (atomSize covrAtom) - fromIntegral (atomDataOffset covrAtom - atomOffset covrAtom)
                if dataSize <= 0
                  then return $ Right Nothing
                  else do
                    atomData <- BS.hGet handle dataSize
                    return $ Right $ parseAlbumArtFull atomData

  case result of
    Left err -> throwError err
    Right maybeArt -> return maybeArt

-- | Parse album art with full image data
parseAlbumArtFull :: BS.ByteString -> Maybe AlbumArt
parseAlbumArtFull bs
  | BS.length bs < 16 = Nothing
  | otherwise =
      let size = runGet getWord32be (L.fromStrict $ BS.take 4 bs)
          dataName = BS.take 4 $ BS.drop 4 bs
      in if dataName /= "data" || size < 16
        then Nothing
        else
          let flags = runGet getWord32be (L.fromStrict $ BS.take 4 $ BS.drop 8 bs)
              imageData = BS.take (fromIntegral size - 16) $ BS.drop 16 bs
              mimeType = case flags of
                13 -> "image/jpeg"  -- JPEG
                14 -> "image/png"   -- PNG
                27 -> "image/bmp"   -- BMP
                _ -> "image/unknown"
          in Just $ AlbumArt
            { albumArtMimeType = mimeType
            , albumArtPictureType = 3  -- Front cover
            , albumArtDescription = ""
            , albumArtData = imageData
            }
