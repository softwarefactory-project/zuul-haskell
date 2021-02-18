{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zuul.Nodeset (Nodeset (..), Node (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data Node = Node
  { nodeName :: Text,
    nodeLabel :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data Nodeset = Nodeset
  { nodesetName :: Text,
    nodesetNodes :: [Node]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Node where
  toJSON = zuulToJSON "node"

instance FromJSON Node where
  parseJSON = zuulParseJSON "node"

instance ToJSON Nodeset where
  toJSON = zuulToJSON "nodeset"

instance FromJSON Nodeset where
  parseJSON = zuulParseJSON "nodeset"
