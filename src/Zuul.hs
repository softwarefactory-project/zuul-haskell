-- | This module contains the zuul REST client
module Zuul
  ( ZuulClient (baseUrl),
    withClient,
    getStatus,
  )
where

import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Zuul.Status

data ZuulClient
  = ZuulClient
      { baseUrl :: Text,
        manager :: Manager
      }

withClient :: Text -> (ZuulClient -> IO ()) -> IO ()
withClient url callBack =
  do
    manager <- newManager tlsManagerSettings
    callBack (ZuulClient {..})
  where
    baseUrl = T.dropWhileEnd (== '/') url <> "/"

zuulGet :: (FromJSON a) => Text -> ZuulClient -> IO a
zuulGet path ZuulClient {..} =
  do
    initRequest <- parseUrlThrow (unpack $ baseUrl <> path)
    let request = initRequest {requestHeaders = [("Accept", "*/*")]}
    response <- httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
      Right a -> pure a

getStatus :: ZuulClient -> IO Status
getStatus = zuulGet "status"
