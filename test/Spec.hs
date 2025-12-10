{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty

import qualified Test.FLACSpec as FLAC
import qualified Test.MP3Spec as MP3
import qualified Test.M4ASpec as M4A
import qualified Test.WriterSpec as Writer
import qualified Test.IntegrationSpec as Integration

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Monatone Parser Tests"
  [ testGroup "Unit Tests"
      [ FLAC.tests
      , MP3.tests
      , M4A.tests
      , Writer.tests
      ]
  , Integration.tests
  ]