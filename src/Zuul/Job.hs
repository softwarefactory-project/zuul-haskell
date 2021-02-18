{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The zuul job data type
module Zuul.Job (Job (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data Job = Job
  { jobName :: Text,
    jobDescription :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Job where
  toJSON = zuulToJSON "job"

instance FromJSON Job where
  parseJSON = zuulParseJSON "job"
