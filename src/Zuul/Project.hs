{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zuul.Project (Project (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data ProjectType = ProjectConfig | ProjectUntrusted
  deriving (Show, Eq, Ord)

instance ToJSON ProjectType where
  toJSON v = case v of
    ProjectConfig -> "config"
    ProjectUntrusted -> "untrusted"

instance FromJSON ProjectType where
  parseJSON (String s) = pure $ case s of
    "config" -> ProjectConfig
    "untrusted" -> ProjectUntrusted
    _ -> error "Received unrecognized ProjectType"
  parseJSON invalid = do
    prependFailure
      "parsing ProjectType failed, "
      (typeMismatch "String" invalid)

data Project = Project
  { projectName :: Text,
    projectType :: ProjectType,
    projectCanonicalName :: Text,
    projectConnectionName :: Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON Project where
  toJSON Project {..} =
    object
      [ "name" .= projectName,
        "type" .= projectType,
        "canonical_name" .= projectCanonicalName,
        "connection_name" .= projectConnectionName
      ]

instance FromJSON Project where
  parseJSON (Object v) = do
    projectName <- v .: "name"
    projectType <- v .: "type"
    projectCanonicalName <- v .: "canonical_name"
    projectConnectionName <- v .: "connection_name"
    pure $ Project {..}
  parseJSON invalid = do
    prependFailure
      "parsing Project failed, "
      (typeMismatch "Object" invalid)
