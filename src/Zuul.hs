{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the zuul REST client
module Zuul
  ( -- * Client
    ZuulClient (baseUrl),
    withClient,
    onTenant,

    -- * Api
    getJobConfig,
    getJobs,
    getProjectConfig,
    getProjects,
    getStatus,
    getTenants,

    -- * Main data types
    Zuul.Job (..),
    Zuul.JobConfig (..),
    Zuul.Node (..),
    Zuul.Nodeset (..),
    Zuul.ProjectPipelineJob (..),
    Zuul.ProjectPipelineConfig (..),
    Zuul.ProjectPipeline (..),
    Zuul.ProjectConfig (..),
    Zuul.Project (..),
    Zuul.Status (..),
    Zuul.SourceContext (..),
    Zuul.Tenant (..),
  )
where

import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Zuul.Job as Zuul
import qualified Zuul.JobConfig as Zuul
import qualified Zuul.Nodeset as Zuul
import qualified Zuul.Project as Zuul
import qualified Zuul.ProjectConfig as Zuul
import qualified Zuul.SourceContext as Zuul
import qualified Zuul.Status as Zuul
import qualified Zuul.Tenant as Zuul

-- | The ZuulClient record, use 'withClient' to create
data ZuulClient = ZuulClient
  { -- | the base url
    baseUrl :: Text,
    manager :: Manager
  }

-- | Create the 'ZuulClient'
withClient ::
  -- | The zuul api url
  Text ->
  -- | The callback
  (ZuulClient -> IO ()) ->
  -- | withClient performs the IO
  IO ()
withClient url callBack =
  do
    manager <- newManager tlsManagerSettings
    callBack (ZuulClient {..})
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

-- | Update a 'ZuulClient' to work on a tenant
onTenant :: ZuulClient -> Text -> ZuulClient
onTenant client tenant = client {baseUrl = baseUrl client <> "tenant/" <> tenant <> "/"}

zuulGet ::
  (FromJSON a) =>
  Text ->
  ZuulClient ->
  IO a
zuulGet path ZuulClient {..} =
  do
    initRequest <- parseUrlThrow (unpack $ baseUrl <> path)
    let request = initRequest {requestHeaders = [("Accept", "*/*")]}
    response <- httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
      Right a -> pure a

-- | Read the status
getStatus :: ZuulClient -> IO Zuul.Status
getStatus = zuulGet "status"

getTenants :: ZuulClient -> IO [Zuul.Tenant]
getTenants = zuulGet "tenants"

getProjects :: ZuulClient -> IO [Zuul.Project]
getProjects = zuulGet "projects"

getProjectConfig :: ZuulClient -> Text -> IO Zuul.ProjectConfig
getProjectConfig client project = zuulGet ("project/" <> project) client

getJobs :: ZuulClient -> IO [Zuul.Job]
getJobs = zuulGet "jobs"

getJobConfig :: ZuulClient -> Text -> IO [Zuul.JobConfig]
getJobConfig client job = zuulGet ("job/" <> job) client
