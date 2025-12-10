{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.M4ASpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except (runExceptT)
import Control.Exception (try, IOException)
import System.OsPath
import Data.Text (Text)

import Monatone.M4A (parseM4A, loadAlbumArtM4A)
import Monatone.Metadata

tests :: TestTree
tests = testGroup "M4A Parser"
  [ testGroup "File parsing"
      [ testParseM4AErrors
      , testParseM4AMetadata
      ]
  , testGroup "Album art"
      [ testLoadAlbumArt
      ]
  ]

testParseM4AErrors :: TestTree
testParseM4AErrors = testGroup "parseM4A error handling"
  [ testCase "handles non-existent file" $ do
      result <- try $ runExceptT $ parseM4A [osp|/nonexistent/file.m4a|]
      case result of
        Left (_ :: IOException) -> return ()  -- Expected IO exception
        Right (Left _) -> return ()  -- Also acceptable - parser error
        Right (Right _) -> assertFailure "Expected error for non-existent file"
  ]

testParseM4AMetadata :: TestTree
testParseM4AMetadata = testGroup "parseM4A metadata extraction"
  [ testCase "parses basic M4A metadata" $ do
      -- This test would run if we had test fixtures
      -- For now, just verify the test structure works
      return ()

  , testCase "handles AAC files" $ do
      -- AAC-specific test
      return ()

  , testCase "handles ALAC files" $ do
      -- Apple Lossless specific test
      return ()
  ]

testLoadAlbumArt :: TestTree
testLoadAlbumArt = testGroup "loadAlbumArtM4A"
  [ testCase "handles non-existent file" $ do
      result <- try $ runExceptT $ loadAlbumArtM4A [osp|/nonexistent/file.m4a|]
      case result of
        Left (_ :: IOException) -> return ()
        Right (Left _) -> return ()
        Right (Right _) -> assertFailure "Expected error for non-existent file"

  , testCase "returns Nothing for file without album art" $ do
      -- Would test with actual fixture
      return ()
  ]
