{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zuul.ProjectConfig
  ( ProjectPipeline (..),
    ProjectPipelineJob (..),
    ProjectPipelineConfig (..),
    ProjectConfig (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)
import Zuul.JobConfig (JobVariables)
import Zuul.Nodeset (Nodeset)
import Zuul.SourceContext (SourceContext)

data ProjectPipelineJob = ProjectPipelineJob
  { ppjName :: Text,
    ppjSourceContext :: SourceContext,
    ppjNodeset :: Maybe Nodeset,
    ppjVariables :: JobVariables,
    ppjExtraVariables :: JobVariables,
    ppjHostVariables :: JobVariables,
    ppjGroupVariables :: JobVariables
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProjectPipelineJob where
  toJSON = zuulToJSON "ppj"

instance FromJSON ProjectPipelineJob where
  parseJSON = zuulParseJSON "ppj"

data ProjectPipeline = ProjectPipeline
  { ppName :: Text,
    ppJobs :: [[ProjectPipelineJob]]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProjectPipeline where
  toJSON = zuulToJSON "pp"

instance FromJSON ProjectPipeline where
  parseJSON = zuulParseJSON "pp"

data ProjectPipelineConfig = ProjectPipelineConfig
  { ppcDefaultBranch :: Maybe Text,
    ppcPipelines :: [ProjectPipeline]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProjectPipelineConfig where
  toJSON = zuulToJSON "ppc"

instance FromJSON ProjectPipelineConfig where
  parseJSON = zuulParseJSON "ppc"

data ProjectConfig = ProjectConfig
  { projectConfigName :: Text,
    projectConfigCanonicalName :: Text,
    projectConfigConnectionName :: Text,
    projectConfigConfigs :: [ProjectPipelineConfig]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ProjectConfig where
  toJSON = zuulToJSON "projectConfig"

instance FromJSON ProjectConfig where
  parseJSON = zuulParseJSON "projectConfig"
