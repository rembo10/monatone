{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.FLACSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import Control.Monad.Except (runExceptT)
import Control.Exception (try, IOException)
import System.OsPath

import Monatone.FLAC (parseFLAC)
import Monatone.Types (readInt, readText, ParseError(..))

tests :: TestTree
tests = testGroup "FLAC Parser"
  [ testGroup "File parsing"
      [ testParseFLACErrors
      ]
  , testGroup "Helper functions"
      [ testReadInt
      , testReadText
      ]
  ]

testParseFLACErrors :: TestTree
testParseFLACErrors = testGroup "parseFLAC error handling"
  [ testCase "handles non-existent file" $ do
      -- The parser throws IO exceptions, so we need to catch them
      result <- try $ runExceptT $ parseFLAC [osp|/nonexistent/file.flac|]
      case result of
        Left (_ :: IOException) -> return ()  -- Expected IO exception
        Right (Left _) -> return ()  -- Also acceptable - parser error
        Right (Right _) -> assertFailure "Expected error for non-existent file"
        
  , testCase "handles non-FLAC file" $ do
      -- Create a temporary non-FLAC file
      let tempFile = "/tmp/test_not_flac.txt"
      writeFile tempFile "This is not a FLAC file"
      
      osPath <- encodeFS tempFile
      result <- runExceptT $ parseFLAC osPath
      case result of
        Left (UnsupportedFormat _) -> return ()  -- Expected error
        Left (CorruptedFile _) -> return ()      -- Also acceptable
        Left otherError -> assertFailure $ "Expected UnsupportedFormat or CorruptedFile, got: " ++ show otherError  
        Right _ -> assertFailure "Expected error for non-FLAC file"
  ]

testReadInt :: TestTree
testReadInt = testGroup "readInt tests"
  [ testCase "parses valid integer" $ do
      readInt "42" @?= Just 42
      readInt "0" @?= Just 0
      readInt "-5" @?= Just (-5)

  , testCase "returns Nothing for invalid integer" $ do
      readInt "abc" @?= Nothing
      readInt "12.34" @?= Nothing
      readInt "" @?= Nothing
      readInt "42x" @?= Nothing
  ]

testReadText :: TestTree
testReadText = testGroup "readText tests"
  [ testCase "decodes valid UTF-8" $ do
      let validUtf8 = BS.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F]  -- "Hello"
      readText validUtf8 @?= Just "Hello"

  , testCase "handles UTF-8 with accents" $ do
      let accentedUtf8 = BS.pack [0xC3, 0xA9]  -- "é"
      readText accentedUtf8 @?= Just "é"

  , testCase "returns Nothing for invalid UTF-8" $ do
      let invalidUtf8 = BS.pack [0xFF, 0xFE]  -- Invalid UTF-8 sequence
      readText invalidUtf8 @?= Nothing

  , testCase "handles empty bytestring" $ do
      readText BS.empty @?= Just ""
  ]