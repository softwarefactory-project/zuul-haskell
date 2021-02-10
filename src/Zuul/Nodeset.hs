{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zuul.Nodeset (Nodeset (..), Node (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data Node = Node
  { nodeName :: Text,
    nodeLabel :: Text
  }
  deriving (Show, Eq, Ord)

instance ToJSON Node where
  toJSON Node {..} =
    object
      [ "name" .= nodeName,
        "label" .= nodeLabel
      ]

instance FromJSON Node where
  parseJSON (Object v) = do
    nodeName <- v .: "name"
    nodeLabel <- v .: "label"
    pure $ Node {..}
  parseJSON invalid =
    prependFailure
      "parsing Node failed, "
      (typeMismatch "Object" invalid)

data Nodeset = Nodeset
  { nodesetName :: Text,
    nodesetNodes :: [Node]
  }
  deriving (Show, Eq, Ord)

instance ToJSON Nodeset where
  toJSON Nodeset {..} =
    object
      [ "name" .= nodesetName,
        "nodes" .= nodesetNodes
      ]

instance FromJSON Nodeset where
  parseJSON (Object v) = do
    nodesetName <- v .: "name"
    nodesetNodes <- v .: "nodes"
    pure $ Nodeset {..}
  parseJSON invalid =
    prependFailure
      "parsing Nodeset failed, "
      (typeMismatch "Object" invalid)
