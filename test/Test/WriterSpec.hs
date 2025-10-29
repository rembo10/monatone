{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.WriterSpec (tests) where

import Test.Tasty  
import Test.Tasty.HUnit

import Monatone.Metadata
import Monatone.Writer

tests :: TestTree
tests = testGroup "Metadata Writer"
  [ testGroup "Writer API"
      [ testMetadataUpdate
      , testBuilderFunctions
      ]
  ]

testMetadataUpdate :: TestTree
testMetadataUpdate = testGroup "MetadataUpdate operations"
  [ testCase "empty update makes no changes" $ do
      let original = emptyMetadata MP3
          update = emptyUpdate
          result = applyUpdate update original
      result @?= original
      
  , testCase "setting title works" $ do
      let original = emptyMetadata MP3
          update = setTitle "Test Title" emptyUpdate
          result = applyUpdate update original
      title result @?= Just "Test Title"
      
  , testCase "clearing title works" $ do
      let original = (emptyMetadata MP3) { title = Just "Original Title" }
          update = clearTitle emptyUpdate
          result = applyUpdate update original
      title result @?= Nothing
      
  , testCase "chaining updates works" $ do
      let original = emptyMetadata MP3
          update = setTitle "Test Title" 
                 $ setArtist "Test Artist"
                 $ setAlbum "Test Album" emptyUpdate
          result = applyUpdate update original
      title result @?= Just "Test Title"
      artist result @?= Just "Test Artist" 
      album result @?= Just "Test Album"
  ]

testBuilderFunctions :: TestTree
testBuilderFunctions = testGroup "Builder functions"
  [ testCase "setTrackNumber works" $ do
      let update = setTrackNumber 5 emptyUpdate
          original = emptyMetadata MP3
          result = applyUpdate update original
      trackNumber result @?= Just 5
      
  , testCase "setYear works" $ do
      let update = setYear 2023 emptyUpdate
          original = emptyMetadata MP3
          result = applyUpdate update original
      year result @?= Just 2023
      
  , testCase "removeAlbumArt works" $ do
      let update = removeAlbumArt emptyUpdate
      -- albumArtInfo is read-only in Metadata, but updateAlbumArt tracks write intent
      updateAlbumArt update @?= Just Nothing
  ]

-- Helper to make applyUpdate accessible for testing
applyUpdate :: MetadataUpdate -> Metadata -> Metadata
applyUpdate update metadata = metadata
  { title = applyMaybeUpdate (updateTitle update) (title metadata)
  , artist = applyMaybeUpdate (updateArtist update) (artist metadata)
  , album = applyMaybeUpdate (updateAlbum update) (album metadata)
  , albumArtist = applyMaybeUpdate (updateAlbumArtist update) (albumArtist metadata)
  , trackNumber = applyMaybeUpdate (updateTrackNumber update) (trackNumber metadata)
  , discNumber = applyMaybeUpdate (updateDiscNumber update) (discNumber metadata)
  , year = applyMaybeUpdate (updateYear update) (year metadata)
  , genre = applyMaybeUpdate (updateGenre update) (genre metadata)
  , publisher = applyMaybeUpdate (updatePublisher update) (publisher metadata)
  , comment = applyMaybeUpdate (updateComment update) (comment metadata)
  -- Note: albumArtInfo is read-only, not updated by this test helper
  }
  where
    applyMaybeUpdate :: Maybe (Maybe a) -> Maybe a -> Maybe a
    applyMaybeUpdate Nothing current = current          -- No change
    applyMaybeUpdate (Just newValue) _ = newValue       -- Apply change (including clearing)
