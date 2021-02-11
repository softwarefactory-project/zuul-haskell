{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON (..), eitherDecodeFileStrict, encodeFile, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Text (encodeToLazyText)
import Data.Map.Strict (Map, fromListWith, toList, unionsWith)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Options.Generic
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath.Posix (splitFileName)
import Zuul
import qualified Zuul.Status

-- | An helper function to cache http query
withCache ::
  (FromJSON a, ToJSON a) =>
  ZuulClient ->
  -- | The query IO
  (ZuulClient -> IO a) ->
  -- | The name of the cache
  Text ->
  IO a
withCache client action cacheName = do
  baseCache <- getUserCacheDir "zuul-haskell"
  let basePath = T.replace "https://" "" (T.replace "http://" "" (baseUrl client))
  let cachePath = baseCache <> "/" <> unpack basePath <> unpack cacheName <> ".json"
  cached <- doesFileExist cachePath
  if cached
    then do
      value <- eitherDecodeFileStrict cachePath
      -- putStrLn ("Reading cache: " <> cachePath)
      case value of
        Left err -> error ("Couldn't decode " <> cachePath <> ":" <> err)
        Right x -> pure x
    else do
      createDirectoryIfMissing True (fst (splitFileName cachePath))
      value <- action client
      putStrLn ("Writting cache: " <> cachePath)
      encodeFile cachePath value
      pure value

-- | Get the list of tenant names
getTenantNames :: ZuulClient -> IO [Text]
getTenantNames client = fmap (fmap tenantName) (withCache client getTenants "tenants")

-- | Get the list of project names
getProjectNames :: Text -> ZuulClient -> IO [Text]
getProjectNames tenant client =
  fmap (fmap projectCanonicalName) (withCache client getProjects (tenant <> "/projects"))

-- | Get the list of job names
getJobNames :: Text -> ZuulClient -> IO [Text]
getJobNames tenant client = fmap (fmap jobName) (withCache client getJobs (tenant <> "/jobs"))

----------------------------------------------------------------------------------------------------
-- Label usages
----------------------------------------------------------------------------------------------------
type LabelName = Text

data LabelUsage = LabelUsage
  { luNodesetName :: Text,
    luTenantName :: Text,
    luJobName :: Text,
    luSourceContext :: SourceContext
  }
  deriving (Show, Eq, Ord)

instance ToJSON LabelUsage where
  toJSON LabelUsage {..} =
    object
      [ "nodeset" .= luNodesetName,
        "tenant" .= luTenantName,
        "job" .= luJobName,
        "source_context" .= luSourceContext
      ]

-- | Convert a nodeset and extra data to a LabelUsage
nodesetToLabel :: Text -> Text -> SourceContext -> Nodeset -> [(LabelName, [LabelUsage])]
nodesetToLabel tenant job sc nodeset = nodeToLabel <$> nodesetNodes nodeset
  where
    nodeToLabel :: Node -> (LabelName, [LabelUsage])
    nodeToLabel Node {..} = (nodeLabel, [LabelUsage (nodesetName nodeset) tenant job sc])

-- | Get LabelUsage of a project
getProjectLabel :: ZuulClient -> Text -> Text -> IO [(LabelName, [LabelUsage])]
getProjectLabel client tenant project = do
  projectConfig <- withCache client (`getProjectConfig` project) (tenant <> "/project/" <> project)
  let jobs :: [ProjectPipelineJob]
      -- this penetrates the project config structure to get all the job in a flat list
      jobs = mconcat $ mconcat $ fmap ppJobs $ mconcat $ fmap ppcPipelines (projectConfigPipelines projectConfig)
  pure $ mconcat $ map projectPipelineJobToLabel jobs
  where
    projectPipelineJobToLabel :: ProjectPipelineJob -> [(LabelName, [LabelUsage])]
    projectPipelineJobToLabel ProjectPipelineJob {..} = case ppjNodeset of
      Nothing -> []
      Just nodeset -> nodesetToLabel tenant ppjName ppjSourceContext nodeset

-- | Get LabelUsage of a job
getJobLabel :: ZuulClient -> Text -> Text -> IO [(LabelName, [LabelUsage])]
getJobLabel client tenant job = do
  jobConfig <- withCache client (`getJobConfig` job) (tenant <> "/job/" <> job)
  pure $ mconcat $ map jobConfigToLabel jobConfig
  where
    jobConfigToLabel :: JobConfig -> [(LabelName, [LabelUsage])]
    jobConfigToLabel JobConfig {..} = case (jcNodeset, jcSourceContext) of
      (Just nodeset, Just sc) -> nodesetToLabel tenant job sc nodeset
      _ -> []

-- | Get LabelUsage of a tenant projects and jobs
getTenantNodepoolLabels :: ZuulClient -> Text -> IO (Map LabelName [LabelUsage])
getTenantNodepoolLabels client tenant = fromListWith (<>) <$> go
  where
    tenantClient = client `onTenant` tenant
    go :: IO [(LabelName, [LabelUsage])]
    go = do
      projectNames <- getProjectNames tenant tenantClient
      projectLabels <- mconcat <$> mapM (getProjectLabel tenantClient tenant) projectNames
      jobNames <- getJobNames tenant tenantClient
      jobLabels <- mconcat <$> mapM (getJobLabel tenantClient tenant) jobNames
      pure $ jobLabels <> projectLabels

-- | Print LabelUsage of one or all tenants
printNodepoolLabels :: Bool -> Text -> Maybe Text -> IO ()
printNodepoolLabels json url tenant = withClient url $ \client -> do
  tenants <- case tenant of
    Nothing -> getTenantNames client
    Just tenant' -> pure [tenant']
  labels <- unionsWith (<>) <$> mapM (getTenantNodepoolLabels client) tenants
  Text.putStrLn $
    if json
      then toStrict (decodeUtf8 (encodePretty labels))
      else T.unlines (concatMap ppr (toList labels))
  where
    ppr :: (LabelName, [LabelUsage]) -> [Text]
    ppr (name, xs) = map (\lu -> name <> " " <> toStrict (encodeToLazyText lu)) xs

----------------------------------------------------------------------------------------------------
-- Live Changes
----------------------------------------------------------------------------------------------------
printLiveChanges :: Text -> Text -> Maybe Text -> IO ()
printLiveChanges url pipeline queue = withClient url $ \client -> do
  status <- getStatus client
  case Zuul.Status.pipelineChanges pipeline queue status of
    Just changes -> print (map Zuul.Status.changeProject (Zuul.Status.liveChanges changes))
    Nothing -> putStrLn "No pipeline found :("

----------------------------------------------------------------------------------------------------
-- Zuul CLI
----------------------------------------------------------------------------------------------------
data ZuulCli w
  = NodepoolLabels
      { url :: w ::: Text <?> "Zuul API url",
        json :: w ::: Bool <?> "Output JSON",
        tenant :: w ::: Maybe Text <?> "A tenant filter"
      }
  | LiveChanges
      { url :: w ::: Text <?> "Zuul API url",
        pipeline :: w ::: Text <?> "Pipeline name",
        queue :: w ::: Maybe Text <?> "Queue name"
      }
  deriving (Generic)

instance ParseRecord (ZuulCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  args <- unwrapRecord "Zuul stats client"
  case args of
    NodepoolLabels {..} -> printNodepoolLabels json url tenant
    LiveChanges {..} -> printLiveChanges url pipeline queue
