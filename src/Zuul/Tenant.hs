{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The zuul tenant data type
module Zuul.Tenant (Tenant (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data Tenant = Tenant
  { tenantName :: Text,
    tenantProjects :: Int,
    tenantQueue :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Tenant where
  toJSON = zuulToJSON "tenant"

instance FromJSON Tenant where
  parseJSON = zuulParseJSON "tenant"
