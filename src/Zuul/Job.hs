{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zuul.Job (Job (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data Job = Job
  { jobName :: Text,
    jobDescription :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON Job where
  toJSON Job {..} =
    object
      [ "name" .= jobName,
        "description" .= jobDescription
      ]

instance FromJSON Job where
  parseJSON (Object v) = do
    jobName <- v .: "name"
    jobDescription <- v .:? "description"
    pure $ Job {..}
  parseJSON invalid =
    prependFailure
      "parsing Job failed, "
      (typeMismatch "Object" invalid)
