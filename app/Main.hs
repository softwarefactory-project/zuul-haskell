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
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map.Strict (Map, fromListWith, toList, unionsWith)
import Data.Maybe (mapMaybe)
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

withTenantCache ::
  (FromJSON a, ToJSON a) =>
  ZuulClient ->
  Text ->
  -- | The query IO
  (ZuulClient -> IO a) ->
  -- | The name of the cache
  Text ->
  IO a
withTenantCache client tenant action cacheName =
  withCache (client `onTenant` tenant) action (tenant <> "/" <> cacheName)

-- | Get the list of tenant names
getTenantNames :: ZuulClient -> Maybe Text -> IO [Text]
getTenantNames client tenantm =
  case tenantm of
    Just tenant -> pure [tenant]
    Nothing -> fmap (fmap tenantName) (withCache client getTenants "tenants")

-- | Get the list of project names
getProjectNames :: ZuulClient -> Text -> IO [Text]
getProjectNames client tenant =
  fmap (fmap projectCanonicalName) (withTenantCache client tenant getProjects "projects")

-- | Get the list of job names
getJobNames :: ZuulClient -> Text -> IO [Text]
getJobNames client tenant = fmap (fmap jobName) (withTenantCache client tenant getJobs "jobs")

-- | Get all the project configs
getProjectConfigs :: ZuulClient -> Text -> IO [ProjectConfig]
getProjectConfigs client tenant = do
  projectNames <- getProjectNames client tenant
  mapM getProjectCache projectNames
  where
    getProjectCache name = withTenantCache client tenant (`getProjectConfig` name) ("project/" <> name)

-- | Get all the jobs
getJobConfigs :: ZuulClient -> Text -> IO [JobConfig]
getJobConfigs client tenant = do
  jobNames <- getJobNames client tenant
  mconcat <$> mapM getJobCache jobNames
  where
    getJobCache name = withTenantCache client tenant (`getJobConfig` name) ("job/" <> name)

getConfigs :: ZuulClient -> Maybe Text -> IO [(Text, [ProjectConfig], [JobConfig])]
getConfigs client tenant = do
  tenants <- getTenantNames client tenant
  zip3 tenants
    <$> mapM (getProjectConfigs client) tenants
    <*> mapM (getJobConfigs client) tenants

getProjectJobs :: ProjectConfig -> [ProjectPipelineJob]
getProjectJobs =
  mconcat
    . mconcat
    . map Zuul.ppJobs
    . mconcat
    . map Zuul.ppcPipelines
    . Zuul.projectConfigConfigs

----------------------------------------------------------------------------------------------------
-- Vars usages
----------------------------------------------------------------------------------------------------
data VarUsage = VarUsage
  { vuTenantName :: Text,
    vuJobName :: Text,
    vuSourceContext :: SourceContext,
    vuVars :: [(Text, JobVariables)]
  }
  deriving (Show, Eq, Ord)

filterEmptyVars :: [(a, Map k b)] -> [(a, Map k b)]
filterEmptyVars = filter (not . M.null . snd)

getProjectJobVars :: Text -> ProjectPipelineJob -> VarUsage
getProjectJobVars tenant ProjectPipelineJob {..} = VarUsage tenant ppjName ppjSourceContext (filterEmptyVars vars)
  where
    vars =
      [ ("vars", ppjVariables),
        ("extra", ppjExtraVariables),
        ("host", ppjHostVariables),
        ("group", ppjGroupVariables)
      ]

getJobVars :: Text -> JobConfig -> Maybe VarUsage
getJobVars tenant JobConfig {..} = case jcSourceContext of
  Nothing -> Nothing
  Just sc -> Just $ VarUsage tenant jcName sc vars
  where
    vars =
      [ ("vars", jcVariables),
        ("extra", jcExtraVariables),
        ("host", jcHostVariables),
        ("group", jcGroupVariables)
      ]

getVars :: (Text, [ProjectConfig], [JobConfig]) -> [VarUsage]
getVars (tenant, pcs, jcs) = filter (not . L.null . vuVars) xs
  where
    xs = map (getProjectJobVars tenant) ppjs <> mapMaybe (getJobVars tenant) jcs
    ppjs = concatMap getProjectJobs pcs

printJobVars :: Maybe Text -> ZuulClient -> IO ()
printJobVars tenant client = do
  configs <- getConfigs client tenant
  let vars = concatMap getVars configs
  mapM_ print vars

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
getProjectLabel :: Text -> ProjectConfig -> [(LabelName, [LabelUsage])]
getProjectLabel tenant pc = mconcat $ map projectPipelineJobToLabel jobs
  where
    jobs :: [ProjectPipelineJob]
    -- this penetrates the project config structure to get all the job in a flat list
    jobs = mconcat $ mconcat $ fmap ppJobs $ mconcat $ fmap ppcPipelines (projectConfigConfigs pc)
    projectPipelineJobToLabel :: ProjectPipelineJob -> [(LabelName, [LabelUsage])]
    projectPipelineJobToLabel ProjectPipelineJob {..} = case ppjNodeset of
      Nothing -> []
      Just nodeset -> nodesetToLabel tenant ppjName ppjSourceContext nodeset

-- | Get LabelUsage of a job
getJobLabel :: Text -> JobConfig -> [(LabelName, [LabelUsage])]
getJobLabel tenant JobConfig {..} = case (jcNodeset, jcSourceContext) of
  (Just nodeset, Just sc) -> nodesetToLabel tenant jcName sc nodeset
  _ -> []

-- | Get LabelUsage of a tenant projects and jobs
getNodepoolLabels :: (Text, [ProjectConfig], [JobConfig]) -> Map LabelName [LabelUsage]
getNodepoolLabels (tenant, pcs, jcs) = fromListWith (<>) labels
  where
    labels :: [(LabelName, [LabelUsage])]
    labels = concatMap (getProjectLabel tenant) pcs <> concatMap (getJobLabel tenant) jcs

-- | Print LabelUsage of one or all tenants
printNodepoolLabels :: Bool -> Maybe Text -> ZuulClient -> IO ()
printNodepoolLabels json tenant client = do
  configs <- getConfigs client tenant
  let labels = unionsWith (<>) (map getNodepoolLabels configs)
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
printLiveChanges :: Text -> Maybe Text -> ZuulClient -> IO ()
printLiveChanges pipeline queue client = do
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
  | JobVars
      { url :: w ::: Text <?> "Zuul API url",
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
    NodepoolLabels {..} -> withClient url (printNodepoolLabels json tenant)
    JobVars {..} -> withClient url (printJobVars tenant)
    LiveChanges {..} -> withClient url (printLiveChanges pipeline queue)
