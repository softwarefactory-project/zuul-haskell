{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zuul.SourceContext (SourceContext (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data SourceContext = SourceContext
  { scBranch :: Text,
    scPath :: Text,
    scProject :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON SourceContext where
  toJSON = zuulToJSON "sc"

instance FromJSON SourceContext where
  parseJSON = zuulParseJSON "sc"
