{-# LANGUAGE RecordWildCards #-}

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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath.Posix (splitFileName)
import Zuul
import qualified Zuul.Status

-- | An helper function to cache http query
withCache ::
  (FromJSON a, ToJSON a) =>
  -- | The query IO
  IO a ->
  -- | The name of the cache
  Text ->
  IO a
withCache action cacheName = do
  baseCache <- getUserCacheDir "zuul-haskell"
  let cachePath = baseCache <> "/" <> unpack cacheName <> ".json"
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
      value <- action
      putStrLn ("Writting cache: " <> cachePath)
      encodeFile cachePath value
      pure value

-- | Get the list of tenant names
getTenantNames :: ZuulClient -> IO [Text]
getTenantNames client = fmap (fmap tenantName) (getTenants client `withCache` "tenants")

-- | Get the list of project names
getProjectNames :: Text -> ZuulClient -> IO [Text]
getProjectNames tenant client = fmap (fmap projectCanonicalName) (getProjects client `withCache` (tenant <> "/projects"))

-- | Get the list of job names
getJobNames :: Text -> ZuulClient -> IO [Text]
getJobNames tenant client = fmap (fmap jobName) (getJobs client `withCache` (tenant <> "/jobs"))

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
  projectConfig <- getProjectConfig client project `withCache` (tenant <> "/project/" <> project)
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
  jobConfig <- getJobConfig client job `withCache` (tenant <> "/job/" <> job)
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

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url, "nodepool-label"] -> printNodepoolLabels False url Nothing
    [url, "nodepool-label", tenant] -> printNodepoolLabels False url (Just tenant)
    [url, "live-changes", pipeline, queue] ->
      withClient url $ \client -> do
        status <- getStatus client
        case Zuul.Status.pipelineChanges pipeline (Just queue) status of
          Just changes -> print (map Zuul.Status.changeProject (Zuul.Status.liveChanges changes))
          Nothing -> putStrLn "No pipeline found :("
    _ -> putStrLn "usage: zuul-cli url pipeline queue"
