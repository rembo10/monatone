{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monatone.M4A.Writer
  ( writeM4AMetadata
  , WriteError(..)
  , Writer
  ) where

import Control.Exception (catch, IOException)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Put
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import System.IO hiding (withBinaryFile)
import System.OsPath
import System.File.OsPath (withBinaryFile)
import System.IO.Temp (withSystemTempFile)

import Monatone.Metadata

-- Re-define WriteError and Writer locally to avoid circular imports
data WriteError
  = WriteIOError Text
  | UnsupportedWriteFormat AudioFormat
  | InvalidMetadata Text
  | CorruptedWrite Text
  deriving (Show, Eq)

type Writer = ExceptT WriteError IO

-- | Write metadata to M4A file
-- M4A writing requires rewriting the entire moov atom, so we use a temp file approach
writeM4AMetadata :: Metadata -> Maybe AlbumArt -> OsPath -> Writer ()
writeM4AMetadata metadata maybeAlbumArt filePath = do
  result <- liftIO $ tryIO $ do
    -- Create temp file
    withSystemTempFile "monatone-m4a.tmp" $ \tmpPath tmpHandle -> do
      hClose tmpHandle  -- Close it so we can use withBinaryFile
      tmpOsPath <- encodeFS tmpPath

      -- Copy file with updated metadata
      runExceptT $ do
        copyM4AWithMetadata filePath tmpOsPath metadata maybeAlbumArt
        -- Copy temp file back to original
        liftIO $ copyFileContents tmpOsPath filePath

  case result of
    Left (e :: IOException) -> throwError $ WriteIOError $ T.pack $ show e
    Right (Left err) -> throwError err
    Right (Right ()) -> return ()
  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO action = catch (Right <$> action) (return . Left)

-- | Copy file contents
copyFileContents :: OsPath -> OsPath -> IO ()
copyFileContents src dst = do
  withBinaryFile src ReadMode $ \srcHandle -> do
    withBinaryFile dst WriteMode $ \dstHandle -> do
      copyLoop srcHandle dstHandle
  where
    copyLoop srcH dstH = do
      chunk <- BS.hGet srcH 65536
      if BS.null chunk
        then return ()
        else do
          BS.hPut dstH chunk
          copyLoop srcH dstH

-- | Copy M4A file with updated metadata
copyM4AWithMetadata :: OsPath -> OsPath -> Metadata -> Maybe AlbumArt -> Writer ()
copyM4AWithMetadata srcPath dstPath metadata maybeAlbumArt = do
  -- Parse source file to get atom structure
  atoms <- liftIO $ withBinaryFile srcPath ReadMode parseTopLevelAtoms

  -- Find moov atom
  case findMoovAtom atoms of
    Nothing -> throwError $ CorruptedWrite "No moov atom found"
    Just (moovOffset, moovSize) -> do
      -- Generate new ilst atom data
      ilstData <- generateIlstData metadata maybeAlbumArt

      -- Write to destination
      _ <- liftIO $ withBinaryFile srcPath ReadMode $ \srcHandle -> do
        withBinaryFile dstPath WriteMode $ \dstHandle -> do
          runExceptT $ rewriteM4AFile srcHandle dstHandle atoms moovOffset moovSize ilstData

      return ()

-- Simple atom info for tracking during parse
data AtomInfo = AtomInfo
  { aiOffset :: Integer
  , aiSize :: Word64
  , aiName :: ByteString
  } deriving (Show)

-- | Parse top-level atoms (simplified, just track positions)
parseTopLevelAtoms :: Handle -> IO [AtomInfo]
parseTopLevelAtoms handle = do
  fileSize <- hFileSize handle
  hSeek handle AbsoluteSeek 0
  parseAtomsLoop handle fileSize []
  where
    parseAtomsLoop h endPos acc = do
      pos <- hTell h
      if pos + 8 > endPos
        then return $ reverse acc
        else do
          header <- BS.hGet h 8
          if BS.length header < 8
            then return $ reverse acc
            else do
              let size32 = readWord32BE $ BS.take 4 header
                  name = BS.take 4 $ BS.drop 4 header

              actualSize <- if size32 == 1
                then do
                  sizeData <- BS.hGet h 8
                  return $ readWord64BE sizeData
                else return $ fromIntegral size32

              let atomInfo = AtomInfo
                    { aiOffset = pos
                    , aiSize = actualSize
                    , aiName = name
                    }

              hSeek h AbsoluteSeek (pos + fromIntegral actualSize)
              parseAtomsLoop h endPos (atomInfo : acc)

-- | Find moov atom
findMoovAtom :: [AtomInfo] -> Maybe (Integer, Word64)
findMoovAtom atoms = case filter (\a -> aiName a == "moov") atoms of
  (moov:_) -> Just (aiOffset moov, aiSize moov)
  [] -> Nothing

-- | Rewrite M4A file with new metadata
rewriteM4AFile :: Handle -> Handle -> [AtomInfo] -> Integer -> Word64 -> L.ByteString -> Writer ()
rewriteM4AFile srcHandle dstHandle atoms moovOffset moovSize newIlstData = do
  -- Copy all atoms before moov
  liftIO $ copyBeforeMoov srcHandle dstHandle atoms moovOffset

  -- Read and rewrite moov atom with new ilst
  liftIO $ do
    hSeek srcHandle AbsoluteSeek moovOffset
    moovData <- BS.hGet srcHandle (fromIntegral moovSize)

    -- Rebuild moov with new ilst
    let newMoovData = rebuildMoovAtom moovData newIlstData
    L.hPut dstHandle newMoovData

  -- Copy all atoms after moov
  liftIO $ copyAfterMoov srcHandle dstHandle moovOffset moovSize

-- | Copy atoms before moov
copyBeforeMoov :: Handle -> Handle -> [AtomInfo] -> Integer -> IO ()
copyBeforeMoov srcHandle dstHandle atoms moovOffset = do
  hSeek srcHandle AbsoluteSeek 0
  let beforeAtoms = filter (\a -> aiOffset a < moovOffset) atoms
  mapM_ (copyAtom srcHandle dstHandle) beforeAtoms

-- | Copy atoms after moov
copyAfterMoov :: Handle -> Handle -> Integer -> Word64 -> IO ()
copyAfterMoov srcHandle dstHandle moovOffset moovSize = do
  let afterOffset = moovOffset + fromIntegral moovSize
  fileSize <- hFileSize srcHandle
  hSeek srcHandle AbsoluteSeek afterOffset

  let remaining = fileSize - afterOffset
  copyBytes srcHandle dstHandle (fromIntegral remaining)

-- | Copy a single atom
copyAtom :: Handle -> Handle -> AtomInfo -> IO ()
copyAtom srcHandle dstHandle atom = do
  hSeek srcHandle AbsoluteSeek (aiOffset atom)
  copyBytes srcHandle dstHandle (fromIntegral $ aiSize atom)

-- | Copy bytes from one handle to another
copyBytes :: Handle -> Handle -> Int -> IO ()
copyBytes srcHandle dstHandle count = go count
  where
    go n
      | n <= 0 = return ()
      | otherwise = do
          let chunkSize = min 65536 n
          chunk <- BS.hGet srcHandle chunkSize
          BS.hPut dstHandle chunk
          go (n - BS.length chunk)

-- | Rebuild moov atom with new ilst data
rebuildMoovAtom :: ByteString -> L.ByteString -> L.ByteString
rebuildMoovAtom oldMoovData newIlstData =
  let newIlst = renderAtom "ilst" newIlstData
      newMeta = renderAtom "meta" (L.fromStrict "\0\0\0\0" <> renderAtom "hdlr" hdlrData <> newIlst)
      newUdta = renderAtom "udta" newMeta

      -- Parse moov children and filter out any existing udta
      moovContentData = BS.drop 8 oldMoovData
      childrenWithoutUdta = filterOutUdta moovContentData

      -- Build new moov with filtered children + new udta
      newMoovContent = childrenWithoutUdta <> newUdta
  in renderAtom "moov" newMoovContent
  where
    hdlrData = L.fromStrict $ BS.pack $ concat
      [ [0,0,0,0, 0,0,0,0]  -- version/flags + reserved
      , map (fromIntegral . fromEnum) "mdirappl"  -- handler_type + reserved
      , [0,0,0,0, 0,0,0,0, 0]  -- reserved
      ]

-- | Filter out udta atom from a sequence of atoms
filterOutUdta :: ByteString -> L.ByteString
filterOutUdta bs = go bs L.empty
  where
    go remaining acc
      | BS.length remaining < 8 = acc
      | otherwise =
          let size32 = readWord32BE $ BS.take 4 remaining
              name = BS.take 4 $ BS.drop 4 remaining

              actualSize = if size32 == 1
                then fromIntegral $ readWord64BE $ BS.take 8 $ BS.drop 8 remaining
                else fromIntegral size32

              atomData = L.fromStrict $ BS.take actualSize remaining
              nextRemaining = BS.drop actualSize remaining

          in if name == "udta"
             then go nextRemaining acc  -- Skip udta atom
             else go nextRemaining (acc <> atomData)  -- Keep other atoms

-- | Generate ilst atom data with all tags
generateIlstData :: Metadata -> Maybe AlbumArt -> Writer L.ByteString
generateIlstData metadata maybeAlbumArt = do
  let tags = concat
        [ renderTextTag "\169nam" <$> maybeToList (title metadata)
        , renderTextTag "\169ART" <$> maybeToList (artist metadata)
        , renderTextTag "\169alb" <$> maybeToList (album metadata)
        , renderTextTag "aART" <$> maybeToList (albumArtist metadata)
        , renderTextTag "\169day" <$> maybeToList (date metadata)
        , renderTextTag "\169gen" <$> maybeToList (genre metadata)
        , renderTextTag "\169cmt" <$> maybeToList (comment metadata)
        , renderTextTag "\169pub" <$> maybeToList (publisher metadata)
        , maybeToList $ renderTrackDiskTag "trkn" (trackNumber metadata) (totalTracks metadata)
        , maybeToList $ renderTrackDiskTag "disk" (discNumber metadata) (totalDiscs metadata)
        , renderCoverTag <$> maybeToList maybeAlbumArt
        -- Freeform tags for MusicBrainz-style metadata
        , renderFreeformTag "LABEL" <$> maybeToList (recordLabel metadata)
        , renderFreeformTag "CATALOGNUMBER" <$> maybeToList (catalogNumber metadata)
        , renderFreeformTag "BARCODE" <$> maybeToList (barcode metadata)
        , renderFreeformTag "MusicBrainz Album Release Country" <$> maybeToList (releaseCountry metadata)
        -- MusicBrainz IDs
        , renderMusicBrainzIds (musicBrainzIds metadata)
        -- Acoustid
        , renderFreeformTag "Acoustid Fingerprint" <$> maybeToList (acoustidFingerprint metadata)
        , renderFreeformTag "Acoustid Id" <$> maybeToList (acoustidId metadata)
        ]

  return $ mconcat tags
  where
    renderMusicBrainzIds mbids = concat
      [ renderFreeformTag "MusicBrainz Release Track Id" <$> maybeToList (mbTrackId mbids)
      , renderFreeformTag "MusicBrainz Track Id" <$> maybeToList (mbRecordingId mbids)
      , renderFreeformTag "MusicBrainz Album Id" <$> maybeToList (mbReleaseId mbids)
      , renderFreeformTag "MusicBrainz Release Group Id" <$> maybeToList (mbReleaseGroupId mbids)
      , renderFreeformTag "MusicBrainz Artist Id" <$> maybeToList (mbArtistId mbids)
      , renderFreeformTag "MusicBrainz Album Artist Id" <$> maybeToList (mbAlbumArtistId mbids)
      , renderFreeformTag "MusicBrainz Work Id" <$> maybeToList (mbWorkId mbids)
      , renderFreeformTag "MusicBrainz Disc Id" <$> maybeToList (mbDiscId mbids)
      ]

-- | Render a text tag atom
renderTextTag :: ByteString -> Text -> L.ByteString
renderTextTag name value =
  let textData = TE.encodeUtf8 value
      dataAtom = renderDataAtom 1 textData  -- Type 1 = UTF-8
  in renderAtom name dataAtom

-- | Render track/disk number tag
renderTrackDiskTag :: ByteString -> Maybe Int -> Maybe Int -> Maybe L.ByteString
renderTrackDiskTag name (Just current) maybeTotal =
  let total = fromMaybe 0 maybeTotal
      trackData = runPut $ do
        putWord16be 0  -- reserved
        putWord16be (fromIntegral current)
        putWord16be (fromIntegral total)
        putWord16be 0  -- reserved
      dataAtom = renderDataAtom 0 (L.toStrict trackData)  -- Type 0 = implicit
  in Just $ renderAtom name dataAtom
renderTrackDiskTag _ Nothing _ = Nothing

-- | Render cover art tag
renderCoverTag :: AlbumArt -> L.ByteString
renderCoverTag art =
  let imageType = case albumArtMimeType art of
        "image/jpeg" -> 13
        "image/png" -> 14
        "image/bmp" -> 27
        _ -> 13  -- Default to JPEG
      dataAtom = renderDataAtom imageType (albumArtData art)
  in renderAtom "covr" dataAtom

-- | Render freeform tag (----:com.apple.iTunes:NAME)
renderFreeformTag :: ByteString -> Text -> L.ByteString
renderFreeformTag name value =
  let mean = "com.apple.iTunes"
      meanAtom = renderAtom "mean" (runPut (putWord32be 0) <> L.fromStrict mean)
      nameAtom = renderAtom "name" (runPut (putWord32be 0) <> L.fromStrict name)
      textData = TE.encodeUtf8 value
      dataAtom = renderDataAtom 1 textData  -- Type 1 = UTF-8
  in renderAtom "----" (meanAtom <> nameAtom <> dataAtom)

-- | Render a data atom
-- Structure: [size:4]['data':4][version/flags:4][reserved?:4][data...]
-- Empirically, there seem to be 16 bytes before data starts (not 12)
renderDataAtom :: Word32 -> ByteString -> L.ByteString
renderDataAtom dataType content =
  let header = runPut $ do
        putWord32be dataType  -- version/flags with type
        putWord32be 0         -- Appears to be a reserved/locale field
  in renderAtom "data" (header <> L.fromStrict content)

-- | Render an atom with name and data
renderAtom :: ByteString -> L.ByteString -> L.ByteString
renderAtom name content =
  let size = 8 + L.length content
      header = runPut $ do
        putWord32be (fromIntegral size)
        putByteString name
  in header <> content

-- Helper functions
readWord32BE :: ByteString -> Word32
readWord32BE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

readWord64BE :: ByteString -> Word64
readWord64BE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word64
      b1 = fromIntegral (BS.index bs 1) :: Word64
      b2 = fromIntegral (BS.index bs 2) :: Word64
      b3 = fromIntegral (BS.index bs 3) :: Word64
      b4 = fromIntegral (BS.index bs 4) :: Word64
      b5 = fromIntegral (BS.index bs 5) :: Word64
      b6 = fromIntegral (BS.index bs 6) :: Word64
      b7 = fromIntegral (BS.index bs 7) :: Word64
  in (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40) .|. (b3 `shiftL` 32) .|.
     (b4 `shiftL` 24) .|. (b5 `shiftL` 16) .|. (b6 `shiftL` 8) .|. b7
