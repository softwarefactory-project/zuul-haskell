{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | The zuul status data type
module Zuul.Status
  ( Job (..),
    Change (..),
    Changes (..),
    ChangeQueue (..),
    Pipeline (..),
    Status (..),
    pipelineChanges,
    liveChanges,
    changeJobUuid,
  )
where

import Control.Monad (guard)
import Data.Aeson (FromJSON, Options (fieldLabelModifier), defaultOptions, genericParseJSON, parseJSON)
import Data.Char (isUpper, toLower)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (id)

-- pipelineChangeQueue -> change_queue
--
zuulParseJSON :: String -> Options
zuulParseJSON prefix = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : updateCase' xs
    updateCase' [] = []
    updateCase' (x : xs)
      | isUpper x = '_' : toLower x : updateCase' xs
      | otherwise = x : updateCase' xs

data Job
  = Job
      { jobName :: Text,
        jobUuid :: Maybe Text,
        jobResult :: Maybe Text
      }
  deriving (Show, Generic)

instance FromJSON Job where
  parseJSON = genericParseJSON $ zuulParseJSON "job"

data Change
  = Change
      { changeId :: Maybe Text,
        changeRef :: Text,
        changeProject :: Text,
        changeLive :: Bool,
        changeActive :: Bool,
        changeJobs :: [Job]
      }
  deriving (Show, Generic)

instance FromJSON Change where
  parseJSON = genericParseJSON $ zuulParseJSON "change"

newtype Changes = Changes [Change]
  deriving (Show, Generic, FromJSON)

data ChangeQueue
  = ChangeQueue
      { changeQueueName :: Text,
        changeQueueHeads :: [Changes]
      }
  deriving (Show, Generic)

instance FromJSON ChangeQueue where
  parseJSON = genericParseJSON $ zuulParseJSON "changeQueue"

data Pipeline
  = Pipeline
      { pipelineName :: Text,
        pipelineChangeQueues :: [ChangeQueue]
      }
  deriving (Show, Generic)

instance FromJSON Pipeline where
  parseJSON = genericParseJSON $ zuulParseJSON "pipeline"

data Status
  = Status
      { statusZuulVersion :: Text,
        statusPipelines :: [Pipeline]
      }
  deriving (Show, Generic)

instance FromJSON Status where
  parseJSON = genericParseJSON $ zuulParseJSON "status"

pipelineChanges :: Text -> Maybe Text -> Status -> Maybe [Change]
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

liveChanges :: [Change] -> [Change]
liveChanges = filter (\c -> changeLive c && changeActive c)

changeJobUuid :: [Change] -> [Text]
changeJobUuid = concatMap go
  where
    go :: Change -> [Text]
    go Change {..} = getUuids changeJobs
    getUuids :: [Job] -> [Text]
    getUuids jobs = do
      job <- jobs
      guard $ isJust (jobUuid job)
      return $ fromJust $ jobUuid job
