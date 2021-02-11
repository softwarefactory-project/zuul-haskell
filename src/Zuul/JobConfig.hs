{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module Zuul.JobConfig (JobConfig (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Zuul.Nodeset (Nodeset)
import Zuul.SourceContext (SourceContext)

data JobConfig = JobConfig
  { jcName :: Text,
    jcSourceContext :: Maybe SourceContext,
    jcDescription :: Maybe Text,
    jcNodeset :: Maybe Nodeset
  }
  deriving (Show, Eq, Ord)

instance ToJSON JobConfig where
  toJSON JobConfig {..} =
    object
      [ "name" .= jcName,
        "source_context" .= jcSourceContext,
        "description" .= jcDescription,
        "nodeset" .= jcNodeset
      ]

instance FromJSON JobConfig where
  parseJSON (Object v) = do
    jcName <- v .: "name"
    jcSourceContext <- v .:? "source_context"
    jcDescription <- v .:? "description"
    jcNodeset <- v .:? "nodeset"
    pure $ JobConfig {..}
  parseJSON invalid =
    prependFailure
      "parsing JobConfig failed, "
      (typeMismatch "Object" invalid)
