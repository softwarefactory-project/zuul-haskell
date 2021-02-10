{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zuul.SourceContext (SourceContext (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data SourceContext = SourceContext
  { scBranch :: Text,
    scPath :: Text,
    scProject :: Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON SourceContext where
  toJSON SourceContext {..} =
    object
      [ "branch" .= scBranch,
        "path" .= scPath,
        "project" .= scProject
      ]

instance FromJSON SourceContext where
  parseJSON (Object v) = do
    scBranch <- v .: "branch"
    scPath <- v .: "path"
    scProject <- v .: "project"
    pure $ SourceContext {..}
  parseJSON invalid = do
    prependFailure
      "parsing SourceContext failed, "
      (typeMismatch "Object" invalid)
