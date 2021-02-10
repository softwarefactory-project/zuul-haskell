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
  parseJSON invalid = do
    prependFailure
      "parsing Node failed, "
      (typeMismatch "Object" invalid)

data Nodeset = Nodeset
  { modelNodes :: [Node]
  }
  deriving (Show, Eq, Ord)

instance ToJSON Nodeset where
  toJSON Nodeset {..} =
    object
      [ "nodes" .= modelNodes
      ]

instance FromJSON Nodeset where
  parseJSON (Object v) = do
    modelNodes <- v .: "nodes"
    pure $ Nodeset {..}
  parseJSON invalid = do
    prependFailure
      "parsing Nodeset failed, "
      (typeMismatch "Object" invalid)
