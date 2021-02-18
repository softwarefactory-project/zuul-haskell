{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The zuul job config data type
module Zuul.JobConfig (JobConfig (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)
import Zuul.Nodeset (Nodeset)
import Zuul.SourceContext (SourceContext)

data JobConfig = JobConfig
  { jcName :: Text,
    jcSourceContext :: Maybe SourceContext,
    jcDescription :: Maybe Text,
    jcNodeset :: Maybe Nodeset
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON JobConfig where
  toJSON = zuulToJSON "jc"

instance FromJSON JobConfig where
  parseJSON = zuulParseJSON "jc"
