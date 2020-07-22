-- | This module contains the zuul REST client
module Zuul
  ( -- * Client
    ZuulClient (baseUrl),
    withClient,

    -- * Api
    getStatus,

    -- * Main data types
    Zuul.Status (..),
  )
where

import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Zuul.Status as Zuul

-- | The ZuulClient record, use 'withClient' to create
data ZuulClient
  = ZuulClient
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
