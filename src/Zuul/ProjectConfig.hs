{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zuul.ProjectConfig (ProjectConfig (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Zuul.Nodeset (Nodeset)
import Zuul.SourceContext (SourceContext)

data ProjectPipelineJob = ProjectPipelineJob
  { ppjName :: Text,
    ppjSourceContext :: SourceContext,
    ppjNodeset :: Maybe Nodeset
  }
  deriving (Show, Eq, Ord)

instance ToJSON ProjectPipelineJob where
  toJSON ProjectPipelineJob {..} =
    object
      [ "name" .= ppjName,
        "source_context" .= ppjSourceContext,
        "nodeset" .= ppjNodeset
      ]

instance FromJSON ProjectPipelineJob where
  parseJSON (Object v) = do
    ppjName <- v .: "name"
    ppjSourceContext <- v .: "source_context"
    ppjNodeset <- v .:? "nodeset"
    pure $ ProjectPipelineJob {..}
  parseJSON invalid =
    prependFailure
      "parsing ProjectPipelineJob failed, "
      (typeMismatch "Object" invalid)

data ProjectPipeline = ProjectPipeline
  { ppName :: Text,
    ppJobs :: [[ProjectPipelineJob]]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ProjectPipeline where
  toJSON ProjectPipeline {..} =
    object
      [ "name" .= ppName,
        "jobs" .= ppJobs
      ]

instance FromJSON ProjectPipeline where
  parseJSON (Object v) = do
    ppName <- v .: "name"
    ppJobs <- v .: "jobs"
    pure $ ProjectPipeline {..}
  parseJSON invalid =
    prependFailure
      "parsing ProjectPipeline failed, "
      (typeMismatch "Object" invalid)

data ProjectPipelineConfig = ProjectPipelineConfig
  { ppcDefaultBranch :: Maybe Text,
    ppcPipelines :: [ProjectPipeline]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ProjectPipelineConfig where
  toJSON ProjectPipelineConfig {..} =
    object
      [ "default_branch" .= ppcDefaultBranch,
        "pipelines" .= ppcPipelines
      ]

instance FromJSON ProjectPipelineConfig where
  parseJSON (Object v) = do
    ppcDefaultBranch <- v .:? "default_branch"
    ppcPipelines <- v .: "pipelines"
    pure $ ProjectPipelineConfig {..}
  parseJSON invalid =
    prependFailure
      "parsing ProjectPipelineConfig failed, "
      (typeMismatch "Object" invalid)

data ProjectConfig = ProjectConfig
  { projectConfigName :: Text,
    projectConfigCanonicalName :: Text,
    projectConfigConnectionName :: Text,
    projectConfigPipelines :: [ProjectPipelineConfig]
  }
  deriving (Show, Eq, Ord)

instance ToJSON ProjectConfig where
  toJSON ProjectConfig {..} =
    object
      [ "name" .= projectConfigName,
        "canonical_name" .= projectConfigCanonicalName,
        "connection_name" .= projectConfigConnectionName,
        "configs" .= projectConfigPipelines
      ]

instance FromJSON ProjectConfig where
  parseJSON (Object v) = do
    projectConfigName <- v .: "name"
    projectConfigCanonicalName <- v .: "canonical_name"
    projectConfigConnectionName <- v .: "connection_name"
    projectConfigPipelines <- v .: "configs"
    pure $ ProjectConfig {..}
  parseJSON invalid =
    prependFailure
      "parsing ProjectConfig failed, "
      (typeMismatch "Object" invalid)
