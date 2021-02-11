{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map.Strict (Map, fromListWith, toList, unionsWith)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath.Posix (splitFileName)
import Zuul
import qualified Zuul.Status

withCache :: (FromJSON a, ToJSON a) => IO a -> Text -> IO a
withCache action cacheName = do
  baseCache <- getUserCacheDir "zuul-haskell"
  let cachePath = baseCache <> "/" <> unpack cacheName <> ".json"
  cached <- doesFileExist cachePath
  if cached
    then do
      value <- eitherDecodeFileStrict cachePath
      putStrLn ("Reading cache: " <> cachePath)
      case value of
        Left err -> error ("Couldn't decode " <> cachePath <> ":" <> err)
        Right x -> pure x
    else do
      createDirectoryIfMissing True (fst $ splitFileName cachePath)
      value <- action
      putStrLn ("Writting cache: " <> cachePath)
      encodeFile cachePath value
      pure value

getTenantNames :: ZuulClient -> IO [Text]
getTenantNames client = fmap (fmap tenantName) (getTenants client `withCache` "tenants")

getProjectNames :: Text -> ZuulClient -> IO [Text]
getProjectNames tenant client = fmap (fmap projectName) (getProjects client `withCache` (tenant <> "/projects"))

type LabelName = Text

data LabelUsage = LabelUsage
  { luJobName :: Text,
    luNodesetName :: Text,
    luTenantName :: Text,
    luSourceContext :: SourceContext
  }
  deriving (Show, Eq, Ord)

getProjectLabel :: ZuulClient -> Text -> Text -> IO [(LabelName, [LabelUsage])]
getProjectLabel client tenant project = do
  projectConfig <- getProjectConfig client project `withCache` (tenant <> "/project/" <> project)
  let ppcs :: [ProjectPipeline]
      ppcs = mconcat $ fmap ppcPipelines (projectConfigPipelines projectConfig)
      jobs :: [ProjectPipelineJob]
      jobs = mconcat $ mconcat $ fmap ppJobs ppcs
  pure $ mconcat $ map projectPipelineJobToLabel jobs
  where
    projectPipelineJobToLabel :: ProjectPipelineJob -> [(LabelName, [LabelUsage])]
    projectPipelineJobToLabel ProjectPipelineJob {..} = case ppjNodeset of
      Nothing -> []
      Just nodeset -> fmap (nodeToLabel (nodesetName nodeset)) (nodesetNodes nodeset)
      where
        nodeToLabel :: Text -> Node -> (LabelName, [LabelUsage])
        nodeToLabel nodesetName' Node {..} = (nodeLabel, [LabelUsage ppjName nodesetName' tenant ppjSourceContext])

getTenantNodepoolLabels :: ZuulClient -> Text -> IO (Map LabelName [LabelUsage])
getTenantNodepoolLabels client tenant = fromListWith (<>) <$> go
  where
    tenantClient = client `onTenant` tenant
    go :: IO [(LabelName, [LabelUsage])]
    go = do
      projectNames <- getProjectNames tenant tenantClient
      mconcat <$> mapM (getProjectLabel tenantClient tenant) projectNames

getNodepoolLabels :: Text -> Maybe Text -> IO ()
getNodepoolLabels url tenant = withClient url $ \client -> do
  tenants <- case tenant of
    Nothing -> getTenantNames client
    Just tenant' -> pure [tenant']
  labels <- unionsWith (<>) <$> mapM (getTenantNodepoolLabels client) tenants
  mapM_ Text.putStrLn (concatMap ppr (toList labels))
  where
    ppr :: (LabelName, [LabelUsage]) -> [Text]
    ppr (name, xs) = map pprUsage xs
      where
        pprUsage :: LabelUsage -> Text
        pprUsage LabelUsage {..} = name <> " " <> luJobName <> " " <> pack (show luSourceContext)

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url, "nodepool-label"] -> getNodepoolLabels url Nothing
    [url, "nodepool-label", tenant] -> getNodepoolLabels url (Just tenant)
    [url, "live-changes", pipeline, queue] ->
      withClient url $ \client -> do
        status <- getStatus client
        case Zuul.Status.pipelineChanges pipeline (Just queue) status of
          Just changes -> print (map Zuul.Status.changeProject (Zuul.Status.liveChanges changes))
          Nothing -> putStrLn "No pipeline found :("
    _ -> putStrLn "usage: zuul-cli url pipeline queue"
