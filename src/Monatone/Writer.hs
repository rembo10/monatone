{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Monatone.Writer
  ( -- * Write errors
    WriteError(..)
  , Writer
    -- * Metadata updates
  , MetadataUpdate(..)
  , emptyUpdate
    -- * Building updates
  , setTitle
  , setArtist
  , setAlbum
  , setAlbumArtist
  , setTrackNumber
  , setDiscNumber
  , setYear
  , setDate
  , setGenre
  , setPublisher
  , setComment
  , setReleaseCountry
  , setLabel
  , setCatalogNumber
  , setBarcode
  , setAlbumArt
    -- * Clearing fields
  , clearTitle
  , clearArtist
  , clearAlbum
  , clearComment
  , removeAlbumArt
    -- * Writing operations
  , writeMetadata
  , writeMetadataToFile
  , updateMetadata
  ) where

import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath
import System.Directory.OsPath (renameFile, removeFile)
import System.File.OsPath (readFile', writeFile')
import Control.Exception (try, IOException, evaluate)

import Monatone.Metadata
import Monatone.Types (Parser, ParseError(..))
import qualified Monatone.MP3 as MP3
import qualified Monatone.FLAC as FLAC
import qualified Monatone.MP3.Writer as MP3Writer
import qualified Monatone.FLAC.Writer as FLACWriter

-- | Write operation errors
data WriteError
  = WriteIOError Text                    -- File I/O error
  | UnsupportedWriteFormat AudioFormat   -- Format not supported for writing
  | InvalidMetadata Text                 -- Metadata validation failed
  | CorruptedWrite Text                  -- Something went wrong during write
  deriving (Show, Eq)

-- | Writer monad for write operations
type Writer = ExceptT WriteError IO

-- | Metadata update specification
-- This represents what changes to make to existing metadata
data MetadataUpdate = MetadataUpdate
  { updateTitle :: Maybe (Maybe Text)        -- Nothing = no change, Just Nothing = clear, Just (Just x) = set to x
  , updateArtist :: Maybe (Maybe Text)
  , updateAlbum :: Maybe (Maybe Text)
  , updateAlbumArtist :: Maybe (Maybe Text)
  , updateTrackNumber :: Maybe (Maybe Int)
  , updateDiscNumber :: Maybe (Maybe Int)
  , updateYear :: Maybe (Maybe Int)
  , updateDate :: Maybe (Maybe Text)
  , updateGenre :: Maybe (Maybe Text)
  , updatePublisher :: Maybe (Maybe Text)
  , updateComment :: Maybe (Maybe Text)
  , updateReleaseCountry :: Maybe (Maybe Text)
  , updateRecordLabel :: Maybe (Maybe Text)
  , updateCatalogNumber :: Maybe (Maybe Text)
  , updateBarcode :: Maybe (Maybe Text)
  , updateAlbumArt :: Maybe (Maybe AlbumArt) -- Nothing = no change, Just Nothing = remove, Just (Just art) = set
  } deriving (Show, Eq)

-- | Empty metadata update (no changes)
emptyUpdate :: MetadataUpdate
emptyUpdate = MetadataUpdate
  { updateTitle = Nothing
  , updateArtist = Nothing
  , updateAlbum = Nothing
  , updateAlbumArtist = Nothing
  , updateTrackNumber = Nothing
  , updateDiscNumber = Nothing
  , updateYear = Nothing
  , updateDate = Nothing
  , updateGenre = Nothing
  , updatePublisher = Nothing
  , updateComment = Nothing
  , updateReleaseCountry = Nothing
  , updateRecordLabel = Nothing
  , updateCatalogNumber = Nothing
  , updateBarcode = Nothing
  , updateAlbumArt = Nothing
  }

-- | Set title
setTitle :: Text -> MetadataUpdate -> MetadataUpdate
setTitle newTitle update = update { updateTitle = Just (Just newTitle) }

-- | Set artist
setArtist :: Text -> MetadataUpdate -> MetadataUpdate
setArtist newArtist update = update { updateArtist = Just (Just newArtist) }

-- | Set album
setAlbum :: Text -> MetadataUpdate -> MetadataUpdate
setAlbum newAlbum update = update { updateAlbum = Just (Just newAlbum) }

-- | Set album artist
setAlbumArtist :: Text -> MetadataUpdate -> MetadataUpdate
setAlbumArtist newAlbumArtist update = update { updateAlbumArtist = Just (Just newAlbumArtist) }

-- | Set track number
setTrackNumber :: Int -> MetadataUpdate -> MetadataUpdate
setTrackNumber newTrackNumber update = update { updateTrackNumber = Just (Just newTrackNumber) }

-- | Set disc number
setDiscNumber :: Int -> MetadataUpdate -> MetadataUpdate
setDiscNumber newDiscNumber update = update { updateDiscNumber = Just (Just newDiscNumber) }

-- | Set year
setYear :: Int -> MetadataUpdate -> MetadataUpdate
setYear newYear update = update { updateYear = Just (Just newYear) }

-- | Set genre
setGenre :: Text -> MetadataUpdate -> MetadataUpdate
setGenre newGenre update = update { updateGenre = Just (Just newGenre) }

-- | Set publisher
setPublisher :: Text -> MetadataUpdate -> MetadataUpdate
setPublisher newPublisher update = update { updatePublisher = Just (Just newPublisher) }

-- | Set comment
setComment :: Text -> MetadataUpdate -> MetadataUpdate
setComment newComment update = update { updateComment = Just (Just newComment) }

-- | Set album art
setAlbumArt :: AlbumArt -> MetadataUpdate -> MetadataUpdate
setAlbumArt art update = update { updateAlbumArt = Just (Just art) }

-- | Set date
setDate :: Text -> MetadataUpdate -> MetadataUpdate
setDate newDate update = update { updateDate = Just (Just newDate) }

-- | Set release country
setReleaseCountry :: Text -> MetadataUpdate -> MetadataUpdate
setReleaseCountry newCountry update = update { updateReleaseCountry = Just (Just newCountry) }

-- | Set record label
setLabel :: Text -> MetadataUpdate -> MetadataUpdate
setLabel newLabel update = update { updateRecordLabel = Just (Just newLabel) }

-- | Set catalog number
setCatalogNumber :: Text -> MetadataUpdate -> MetadataUpdate
setCatalogNumber newCatalog update = update { updateCatalogNumber = Just (Just newCatalog) }

-- | Set barcode
setBarcode :: Text -> MetadataUpdate -> MetadataUpdate
setBarcode newBarcode update = update { updateBarcode = Just (Just newBarcode) }

-- | Clear title field
clearTitle :: MetadataUpdate -> MetadataUpdate
clearTitle update = update { updateTitle = Just Nothing }

-- | Clear artist field  
clearArtist :: MetadataUpdate -> MetadataUpdate
clearArtist update = update { updateArtist = Just Nothing }

-- | Clear album field
clearAlbum :: MetadataUpdate -> MetadataUpdate  
clearAlbum update = update { updateAlbum = Just Nothing }

-- | Clear comment field
clearComment :: MetadataUpdate -> MetadataUpdate
clearComment update = update { updateComment = Just Nothing }

-- | Remove album art
removeAlbumArt :: MetadataUpdate -> MetadataUpdate
removeAlbumArt update = update { updateAlbumArt = Just Nothing }

-- | Apply metadata update to existing metadata
applyUpdate :: MetadataUpdate -> Metadata -> Metadata
applyUpdate update metadata =
  let !fmt = format metadata
      !props = audioProperties metadata
      !mbids = musicBrainzIds metadata
      !acoustFP = acoustidFingerprint metadata
      !acoustID = acoustidId metadata
      !tags = rawTags metadata
      !totTracks = totalTracks metadata
      !totDiscs = totalDiscs metadata
      !relStatus = releaseStatus metadata
      !relType = releaseType metadata
      -- Album art info is read-only (no writing support for now, as Metadata only stores info not data)
      !artInfo = albumArtInfo metadata
  in Metadata
    { format = fmt
    , title = applyMaybeUpdate (updateTitle update) (title metadata)
    , artist = applyMaybeUpdate (updateArtist update) (artist metadata)
    , album = applyMaybeUpdate (updateAlbum update) (album metadata)
    , albumArtist = applyMaybeUpdate (updateAlbumArtist update) (albumArtist metadata)
    , trackNumber = applyMaybeUpdate (updateTrackNumber update) (trackNumber metadata)
    , totalTracks = totTracks
    , discNumber = applyMaybeUpdate (updateDiscNumber update) (discNumber metadata)
    , totalDiscs = totDiscs
    , date = applyMaybeUpdate (updateDate update) (date metadata)
    , year = applyMaybeUpdate (updateYear update) (year metadata)
    , genre = applyMaybeUpdate (updateGenre update) (genre metadata)
    , publisher = applyMaybeUpdate (updatePublisher update) (publisher metadata)
    , comment = applyMaybeUpdate (updateComment update) (comment metadata)
    , releaseCountry = applyMaybeUpdate (updateReleaseCountry update) (releaseCountry metadata)
    , recordLabel = applyMaybeUpdate (updateRecordLabel update) (recordLabel metadata)
    , catalogNumber = applyMaybeUpdate (updateCatalogNumber update) (catalogNumber metadata)
    , barcode = applyMaybeUpdate (updateBarcode update) (barcode metadata)
    , releaseStatus = relStatus
    , releaseType = relType
    , albumArtInfo = artInfo  -- Read-only, no writing support
    , audioProperties = props
    , musicBrainzIds = mbids
    , acoustidFingerprint = acoustFP
    , acoustidId = acoustID
    , rawTags = tags
    }
  where
    applyMaybeUpdate :: Maybe (Maybe a) -> Maybe a -> Maybe a
    applyMaybeUpdate Nothing current = current          -- No change
    applyMaybeUpdate (Just newValue) _ = newValue       -- Apply change (including clearing)

-- | Write complete metadata to a new file
writeMetadata :: Metadata -> OsPath -> Writer ()
writeMetadata metadata filePath = do
  let audioFormat = format metadata
  case audioFormat of
    MP3 -> writeMP3Metadata metadata filePath
    FLAC -> writeFLACMetadata metadata filePath
    _ -> throwError $ UnsupportedWriteFormat audioFormat

-- | Write metadata to the same file (with backup)
writeMetadataToFile :: Metadata -> OsPath -> Writer ()
writeMetadataToFile metadata filePath = do
  -- Create backup path by appending .backup to filename
  let backupPath = filePath <> [osp|.backup|]
  
  -- Create backup by copying (not renaming) so original stays available for writers
  backupResult <- liftIO $ try $ do
    content <- readFile' filePath
    writeFile' backupPath content
  case backupResult of
    Left (ioErr :: IOException) -> throwError $ WriteIOError $ "Failed to create backup: " <> T.pack (show ioErr)
    Right _ -> do
      -- Try to write new file
      writeResult <- liftIO $ runExceptT $ writeMetadata metadata filePath
      case writeResult of
        Left err -> do
          -- Restore backup on failure
          restoreResult <- liftIO $ try $ renameFile backupPath filePath
          case restoreResult of
            Left (restoreErr :: IOException) -> 
              throwError $ WriteIOError $ "Write failed and backup restore failed: " <> T.pack (show restoreErr)
            Right _ -> throwError err
        Right _ -> do
          -- Success - clean up backup
          cleanupResult <- liftIO $ (try :: IO () -> IO (Either IOException ())) $ removeFile backupPath
          case cleanupResult of
            Left _ -> return ()  -- Ignore cleanup errors
            Right _ -> return ()

-- | Update existing file with metadata changes
updateMetadata :: OsPath -> MetadataUpdate -> Writer ()
updateMetadata filePath update = do
  -- Read existing metadata
  existingResult <- liftIO $ runExceptT $ parseFile filePath
  case existingResult of
    Left parseErr -> throwError $ CorruptedWrite $ "Failed to read existing metadata: " <> T.pack (show parseErr)
    Right existingMetadata -> do
      -- Apply update
      let updatedMetadata = applyUpdate update existingMetadata
      -- Force evaluation of the format field to ensure metadata is constructed
      _ <- liftIO $ evaluate (format updatedMetadata)
      -- Write back
      writeMetadataToFile updatedMetadata filePath

-- | Write MP3 metadata using the MP3Writer module
writeMP3Metadata :: Metadata -> OsPath -> Writer ()
writeMP3Metadata metadata filePath = do
  result <- liftIO $ runExceptT $ MP3Writer.writeMP3Metadata metadata filePath
  case result of
    Left mp3Err -> throwError $ convertMP3Error mp3Err
    Right () -> return ()
  where
    convertMP3Error :: MP3Writer.WriteError -> WriteError
    convertMP3Error (MP3Writer.WriteIOError msg) = WriteIOError msg
    convertMP3Error (MP3Writer.UnsupportedWriteFormat fmt) = UnsupportedWriteFormat fmt
    convertMP3Error (MP3Writer.InvalidMetadata msg) = InvalidMetadata msg
    convertMP3Error (MP3Writer.CorruptedWrite msg) = CorruptedWrite msg

-- | Write FLAC metadata using the FLACWriter module
writeFLACMetadata :: Metadata -> OsPath -> Writer ()
writeFLACMetadata metadata filePath = do
  result <- liftIO $ runExceptT $ FLACWriter.writeFLACMetadata metadata filePath
  case result of
    Left flacErr -> throwError $ convertFLACError flacErr
    Right () -> return ()
  where
    convertFLACError :: FLACWriter.WriteError -> WriteError
    convertFLACError (FLACWriter.WriteIOError msg) = WriteIOError msg
    convertFLACError (FLACWriter.UnsupportedWriteFormat fmt) = UnsupportedWriteFormat fmt
    convertFLACError (FLACWriter.InvalidMetadata msg) = InvalidMetadata msg
    convertFLACError (FLACWriter.CorruptedWrite msg) = CorruptedWrite msg

-- | Parse file using existing parsers based on extension
parseFile :: OsPath -> Parser Metadata  
parseFile filePath = do
  -- Convert extension to lowercase for comparison
  let ext = takeExtension filePath
  if ext == [osp|.mp3|] || ext == [osp|.MP3|]
    then MP3.parseMP3 filePath
    else if ext == [osp|.flac|] || ext == [osp|.FLAC|]
    then FLAC.parseFLAC filePath  
    else throwError $ UnsupportedFormat "Unsupported file extension"