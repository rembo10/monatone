{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.IntegrationSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except (runExceptT)
import System.FilePath ((</>))
import System.Directory (doesFileExist, copyFile, removeFile, getTemporaryDirectory)
import System.Process (callProcess)
import Control.Exception (catch, SomeException)
import Control.Monad (unless)
import System.OsPath hiding ((</>))
import qualified Data.Text as T

import Monatone.Common (parseMetadata)
import Monatone.Metadata
import Monatone.Writer

tests :: TestTree
tests = withResource ensureFixtures (const $ return ()) $ \_ ->
  testGroup "Integration Tests"
    [ testGroup "Reading Real Files"
        [ testReadMinimalMP3
        , testReadTaggedMP3
        , testReadMinimalFLAC
        ]
    , testGroup "Round-trip Tests"
        [ testMP3RoundTrip
        , testFLACRoundTrip
        ]
    ]

-- | Ensure test fixtures exist, generate them if missing
ensureFixtures :: IO ()
ensureFixtures = do
  let files = [ fixturesDir </> "minimal.mp3"
              , fixturesDir </> "tagged.mp3"
              , fixturesDir </> "minimal.flac"
              ]
  allExist <- and <$> mapM doesFileExist files
  unless allExist $ do
    putStrLn "Test fixtures not found. Attempting to generate with ffmpeg..."
    generateTestFiles `catch` handleError
  where
    handleError :: SomeException -> IO ()
    handleError _ = putStrLn "Warning: Could not generate test fixtures (ffmpeg not available). Integration tests will be skipped."

-- | Generate test files using ffmpeg
generateTestFiles :: IO ()
generateTestFiles = do
  -- Generate 1 second of silence as raw PCM
  callProcess "ffmpeg" ["-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo", 
                        "-t", "1", "-f", "s16le", "-y", "/tmp/silence.raw"]
  
  -- Create MP3 with metadata
  callProcess "ffmpeg" ["-f", "s16le", "-ar", "44100", "-ac", "2", "-i", "/tmp/silence.raw",
                        "-codec:a", "libmp3lame", "-b:a", "128k",
                        "-metadata", "title=Test Title",
                        "-metadata", "artist=Test Artist",
                        "-metadata", "album=Test Album",
                        "-metadata", "date=2024",
                        "-metadata", "track=1/10",
                        "-metadata", "genre=Rock",
                        "-metadata", "comment=Test comment",
                        "-y", fixturesDir </> "tagged.mp3"]
  
  -- Create minimal MP3
  callProcess "ffmpeg" ["-f", "s16le", "-ar", "44100", "-ac", "2", "-i", "/tmp/silence.raw",
                        "-codec:a", "libmp3lame", "-b:a", "128k",
                        "-metadata", "title=Minimal Title",
                        "-y", fixturesDir </> "minimal.mp3"]
  
  -- Create FLAC with metadata
  callProcess "ffmpeg" ["-f", "s16le", "-ar", "44100", "-ac", "2", "-i", "/tmp/silence.raw",
                        "-codec:a", "flac",
                        "-metadata", "title=FLAC Test Track",
                        "-metadata", "artist=Test Band",
                        "-metadata", "album=FLAC Album",
                        "-metadata", "date=2024",
                        "-metadata", "track=3",
                        "-metadata", "comment=FLAC test comment",
                        "-y", fixturesDir </> "minimal.flac"]
  
  -- Clean up
  removeFile "/tmp/silence.raw" `catch` (\(_ :: SomeException) -> return ())

fixturesDir :: FilePath
fixturesDir = "test/fixtures"

-- Helper to convert FilePath to OsPath
toOsPath :: FilePath -> IO OsPath
toOsPath = encodeFS

testReadMinimalMP3 :: TestTree
testReadMinimalMP3 = testCase "Read minimal MP3" $ do
  let path = fixturesDir </> "minimal.mp3"
  exists <- doesFileExist path
  unless exists $ assertFailure "Test skipped: fixture not available (run with ffmpeg to generate)"
  
  osPath <- toOsPath path
  result <- parseMetadata osPath
  case result of
    Left err -> assertFailure $ T.unpack $ "Failed to parse: " <> T.pack (show err)
    Right metadata -> do
      assertEqual "Format" MP3 (format metadata)
      assertEqual "Title" (Just "Minimal Title") (title metadata)

testReadTaggedMP3 :: TestTree
testReadTaggedMP3 = testCase "Read MP3 with full metadata" $ do
  let path = fixturesDir </> "tagged.mp3"
  exists <- doesFileExist path
  assertBool "Test file exists" exists
  
  osPath <- toOsPath path
  result <- parseMetadata osPath
  case result of
    Left err -> assertFailure $ T.unpack $ "Failed to parse: " <> T.pack (show err)
    Right metadata -> do
      assertEqual "Format" MP3 (format metadata)
      assertEqual "Title" (Just "Test Title") (title metadata)
      assertEqual "Artist" (Just "Test Artist") (artist metadata)
      assertEqual "Album" (Just "Test Album") (album metadata)
      assertEqual "Year" (Just 2024) (year metadata)
      assertEqual "Track number" (Just 1) (trackNumber metadata)
      assertEqual "Genre" (Just "Rock") (genre metadata)
      assertEqual "Comment" (Just "Test comment") (comment metadata)

testReadMinimalFLAC :: TestTree
testReadMinimalFLAC = testCase "Read minimal FLAC" $ do
  let path = fixturesDir </> "minimal.flac"
  exists <- doesFileExist path
  assertBool "Test file exists" exists
  
  osPath <- toOsPath path
  result <- parseMetadata osPath
  case result of
    Left err -> assertFailure $ T.unpack $ "Failed to parse: " <> T.pack (show err)
    Right metadata -> do
      assertEqual "Format" FLAC (format metadata)
      assertEqual "Title" (Just "FLAC Test Track") (title metadata)
      assertEqual "Artist" (Just "Test Band") (artist metadata)
      
      -- Check audio properties from STREAMINFO
      let props = audioProperties metadata
      case sampleRate props of
        Just sr -> assertEqual "Sample rate" 44100 sr
        Nothing -> assertFailure "No sample rate found"
      case channels props of
        Just ch -> assertEqual "Channels" 2 ch
        Nothing -> assertFailure "No channels found"

testMP3RoundTrip :: TestTree
testMP3RoundTrip = testCase "MP3 read-write-read round trip" $ do
  -- Use system temp directory
  tmpDir <- getTemporaryDirectory
  let origPath = fixturesDir </> "tagged.mp3"
  let tmpPath = tmpDir </> "monatone-test-mp3.mp3"
  
  -- Verify source file exists
  origExists <- doesFileExist origPath
  assertBool (T.unpack $ "Source file exists: " <> T.pack origPath) origExists
  
  -- Copy file to temp location
  copyFile origPath tmpPath
  
  -- Verify copy succeeded
  tmpExists <- doesFileExist tmpPath
  assertBool (T.unpack $ "Temp file created: " <> T.pack tmpPath) tmpExists
  
  -- Read original metadata
  osTmpPath <- toOsPath tmpPath
  origResult <- parseMetadata osTmpPath
  origMetadata <- case origResult of
    Left err -> assertFailure $ T.unpack $ "Failed to read original: " <> T.pack (show err)
    Right m -> return m
  
  -- Modify metadata
  let update = setTitle "Modified Title" $
               setArtist "Modified Artist" $
               setYear 2025 $
               emptyUpdate
  
  writeResult <- runExceptT $ updateMetadata osTmpPath update
  case writeResult of
    Left err -> assertFailure $ "Failed to write: " ++ show err
    Right () -> return ()
  
  -- Read modified metadata
  modResult <- parseMetadata osTmpPath
  case modResult of
    Left err -> assertFailure $ "Failed to read modified: " ++ show err
    Right modMetadata -> do
      assertEqual "Modified title" (Just "Modified Title") (title modMetadata)
      assertEqual "Modified artist" (Just "Modified Artist") (artist modMetadata)
      assertEqual "Modified year" (Just 2025) (year modMetadata)
      -- Unchanged fields should be preserved
      assertEqual "Album preserved" (album origMetadata) (album modMetadata)
      assertEqual "Genre preserved" (genre origMetadata) (genre modMetadata)
  
  -- Clean up
  removeFile tmpPath

testFLACRoundTrip :: TestTree
testFLACRoundTrip = testCase "FLAC read-write-read round trip" $ do
  -- Use system temp directory
  tmpDir <- getTemporaryDirectory
  let origPath = fixturesDir </> "minimal.flac"
  let tmpPath = tmpDir </> "monatone-test-flac.flac"
  
  -- Verify source file exists
  origExists <- doesFileExist origPath
  assertBool (T.unpack $ "Source file exists: " <> T.pack origPath) origExists
  
  -- Copy file to temp location
  copyFile origPath tmpPath
  
  -- Verify copy succeeded
  tmpExists <- doesFileExist tmpPath
  assertBool (T.unpack $ "Temp file created: " <> T.pack tmpPath) tmpExists
  
  -- Read original metadata
  osTmpPath <- toOsPath tmpPath
  origResult <- parseMetadata osTmpPath
  origMetadata <- case origResult of
    Left err -> assertFailure $ T.unpack $ "Failed to read original: " <> T.pack (show err)
    Right m -> return m
  
  -- Modify metadata
  let update = setTitle "New FLAC Title" $
               setAlbum "New Album" $
               setTrackNumber 5 $
               setComment "Test comment" $
               emptyUpdate
  
  writeResult <- runExceptT $ updateMetadata osTmpPath update
  case writeResult of
    Left err -> assertFailure $ "Failed to write: " ++ show err
    Right () -> return ()
  
  -- Read modified metadata
  modResult <- parseMetadata osTmpPath
  case modResult of
    Left err -> assertFailure $ "Failed to read modified: " ++ show err
    Right modMetadata -> do
      assertEqual "Modified title" (Just "New FLAC Title") (title modMetadata)
      assertEqual "Modified album" (Just "New Album") (album modMetadata)
      assertEqual "Modified track" (Just 5) (trackNumber modMetadata)
      assertEqual "Modified comment" (Just "Test comment") (comment modMetadata)
      -- Original artist should be preserved
      assertEqual "Artist preserved" (artist origMetadata) (artist modMetadata)
      -- Audio properties should remain unchanged
      assertEqual "Audio props preserved" 
        (audioProperties origMetadata) 
        (audioProperties modMetadata)
  
  -- Clean up
  removeFile tmpPath
