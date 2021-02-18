{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zuul.Project (Project (..), ProjectType (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data ProjectType = ProjectConfig | ProjectUntrusted
  deriving (Show, Eq, Ord, Generic)

data Project = Project
  { projectName :: Text,
    projectType :: ProjectType,
    projectCanonicalName :: Text,
    projectConnectionName :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProjectType where
  toJSON v = case v of
    ProjectConfig -> "config"
    ProjectUntrusted -> "untrusted"

instance FromJSON ProjectType where
  parseJSON (String s) = pure $ case s of
    "config" -> ProjectConfig
    "untrusted" -> ProjectUntrusted
    _ -> error "Received unrecognized ProjectType"
  parseJSON invalid =
    prependFailure
      "parsing ProjectType failed, "
      (typeMismatch "String" invalid)

instance ToJSON Project where
  toJSON = zuulToJSON "project"

instance FromJSON Project where
  parseJSON = zuulParseJSON "project"
