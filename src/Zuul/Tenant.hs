{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The zuul tenant data type
module Zuul.Tenant (Tenant (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data Tenant = Tenant
  { tenantName :: Text,
    tenantProjects :: Int,
    tenantQueue :: Int
  }
  deriving (Show, Eq, Ord)

instance ToJSON Tenant where
  toJSON Tenant {..} =
    object
      [ "name" .= tenantName,
        "projects" .= tenantProjects,
        "queue" .= tenantQueue
      ]

instance FromJSON Tenant where
  parseJSON (Object v) = do
    tenantName <- v .: "name"
    tenantProjects <- v .: "projects"
    tenantQueue <- v .: "queue"
    pure $ Tenant {..}
  parseJSON invalid =
    prependFailure
      "parsing Tenant failed, "
      (typeMismatch "Object" invalid)
