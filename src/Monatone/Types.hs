{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Monatone.Types
  ( ParseError(..)
  , Parser
  , readInt
  , readText
  , formatError  -- Export error formatter
  ) where

import Control.Monad.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Parser errors with detailed context
-- 
-- These errors provide enough information for debugging without
-- exposing internal implementation details to end users.
data ParseError 
  = UnsupportedFormat Text        -- ^ File format not supported (with format details)
  | CorruptedFile Text            -- ^ File structure invalid (with corruption details)
  | IOError Text                  -- ^ IO operation failed (with system error)
  | PartialParse Text             -- ^ File partially parsed (common with corrupted MP3s)
  | InvalidEncoding Text          -- ^ Text encoding issues (common with old MP3 tags) 
  deriving (Show, Eq, Generic)

-- | Format error for user display
formatError :: ParseError -> Text
formatError (UnsupportedFormat detail) = "Unsupported format: " <> detail
formatError (CorruptedFile detail) = "Corrupted file: " <> detail  
formatError (IOError detail) = "IO error: " <> detail
formatError (PartialParse detail) = "Warning: " <> detail <> " (partial data recovered)"
formatError (InvalidEncoding detail) = "Encoding error: " <> detail

-- | Parser monad
type Parser = ExceptT ParseError IO

-- | Helper: Parse integer from text
readInt :: Text -> Maybe Int
readInt s = case reads (T.unpack s) of
  [(x, "")] -> Just x
  _ -> Nothing

-- | Helper: Parse text with UTF-8 decoding
readText :: ByteString -> Maybe Text
readText bs = case TE.decodeUtf8' bs of
  Left _ -> Nothing
  Right txt -> Just txt