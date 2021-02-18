{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The zuul status data type
module Zuul.Status
  ( -- * Status data types
    JobStatus (..),
    Change (..),
    Changes (..),
    ChangeQueue (..),
    Pipeline (..),
    Status (..),

    -- * Convenient functions
    pipelineChanges,
    liveChanges,
    changeJobUuid,
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import Zuul.Aeson (zuulParseJSON, zuulToJSON)

data JobStatus = JobStatus
  { jobName :: Text,
    jobUuid :: Maybe Text,
    jobResult :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON JobStatus where
  parseJSON = zuulParseJSON "job"

instance ToJSON JobStatus where
  toJSON = zuulToJSON "job"

data Change = Change
  { changeId :: Maybe Text,
    changeRef :: Text,
    changeProject :: Text,
    changeLive :: Bool,
    changeActive :: Bool,
    changeJobs :: [JobStatus]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Change where
  parseJSON = zuulParseJSON "change"

instance ToJSON Change where
  toJSON = zuulToJSON "change"

newtype Changes = Changes [Change]
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data ChangeQueue = ChangeQueue
  { changeQueueName :: Text,
    changeQueueHeads :: [Changes]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ChangeQueue where
  parseJSON = zuulParseJSON "changeQueue"

instance ToJSON ChangeQueue where
  toJSON = zuulToJSON "changeQueue"

data Pipeline = Pipeline
  { pipelineName :: Text,
    pipelineChangeQueues :: [ChangeQueue]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Pipeline where
  parseJSON = zuulParseJSON "pipeline"

instance ToJSON Pipeline where
  toJSON = zuulToJSON "pipeline"

data Status = Status
  { statusZuulVersion :: Text,
    statusPipelines :: [Pipeline]
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Status where
  parseJSON = zuulParseJSON "status"

instance ToJSON Status where
  toJSON = zuulToJSON "status"

-- | Get the change from a pipeline
pipelineChanges ::
  -- | The pipeline name
  Text ->
  -- | An optional queue name
  Maybe Text ->
  -- | The status record
  Status ->
  -- | Returns an optional list of changes
  Maybe [Change]
pipelineChanges name queueName status =
  case filter (\c -> pipelineName c == name) (statusPipelines status) of
    [pipeline] -> Just $ processPipeline pipeline
    _ -> Nothing
  where
    processPipeline :: Pipeline -> [Change]
    processPipeline pipeline = concatMap processQueue (pipelineChangeQueues pipeline)
    processQueue :: ChangeQueue -> [Change]
    processQueue queue = case queueName of
      Just queueName' ->
        if changeQueueName queue == queueName'
          then concatMap processChanges (changeQueueHeads queue)
          else []
      Nothing -> concatMap processChanges (changeQueueHeads queue)
    processChanges :: Changes -> [Change]
    processChanges (Changes changes) = changes

-- | Filter the change that are live and active
liveChanges :: [Change] -> [Change]
liveChanges = filter (\c -> changeLive c && changeActive c)

-- | Extract the job uuids from a list of change
changeJobUuid :: [Change] -> [Text]
changeJobUuid = concatMap go
  where
    go :: Change -> [Text]
    go Change {..} = getUuids changeJobs
    getUuids :: [JobStatus] -> [Text]
    getUuids jobs = do
      job <- jobs
      guard $ isJust (jobUuid job)
      return $ fromJust $ jobUuid job
