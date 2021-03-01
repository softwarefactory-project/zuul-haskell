{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The zuul job config data type
module Zuul.JobConfig (JobConfig (..), JobVariables) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)
import Zuul.Nodeset (Nodeset)
import Zuul.SourceContext (SourceContext)

type JobVariables = Map Text Value

data JobConfig = JobConfig
  { jcName :: Text,
    jcSourceContext :: Maybe SourceContext,
    jcDescription :: Maybe Text,
    jcNodeset :: Maybe Nodeset,
    jcVariables :: JobVariables,
    jcExtraVariables :: JobVariables,
    jcHostVariables :: JobVariables,
    jcGroupVariables :: JobVariables
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON JobConfig where
  toJSON = zuulToJSON "jc"

instance FromJSON JobConfig where
  parseJSON = zuulParseJSON "jc"
