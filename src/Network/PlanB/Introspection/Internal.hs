{-# LANGUAGE OverloadedStrings #-}

module Network.PlanB.Introspection.Internal
  ( TokenInfo(..)
  , Conf
  , PlanBIntrospectionException
  , newConf
  , newConfIO
  , httpRequestExecuteIO
  , introspectToken
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString.Lazy                       as ByteString.Lazy
import           Data.Function                              ((&))
import           Data.Monoid
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

import           Network.PlanB.Introspection.Internal.Types

newConf
  :: MonadThrow m
  => (Request -> m (Response LazyByteString))
  -> Text
  -> m (Conf m)
newConf httpRequestExecute introspectionEndpoint = do
  introspectionRequest <- parseRequest introspectionEndpointStr
  pure Conf { confIntrospectionRequest = introspectionRequest
            , confHttpRequestExecute   = httpRequestExecute }
  where introspectionEndpointStr = Text.unpack introspectionEndpoint

newConfIO
  :: (MonadThrow m, MonadIO m)
  => Text
  -> m (Conf m)
newConfIO introspectionEndpoint = do
  introspectionRequest <- parseRequest introspectionEndpointStr
  pure Conf { confIntrospectionRequest = introspectionRequest
            , confHttpRequestExecute   = httpRequestExecuteIO Nothing }
  where introspectionEndpointStr = Text.unpack introspectionEndpoint

httpRequestExecuteIO
  :: MonadIO m
  => Maybe Manager
  -> Request
  -> m (Response LazyByteString)
httpRequestExecuteIO maybeManager request = do
  liftIO $ print request
  manager <- maybe (liftIO getGlobalManager) pure maybeManager
  liftIO $ httpLbs request manager

introspectToken
  :: MonadThrow m
  => Conf m
  -> ByteString
  -> m TokenInfo
introspectToken conf token = do
  let endpoint    = confIntrospectionRequest conf
      bearerToken = "Bearer " <> token
      request     = endpoint { method         = "GET"
                             , path           = "/oauth2/tokeninfo"
                             , requestHeaders = [("Authorization", bearerToken)] }
  response <- confHttpRequestExecute conf request
  let body = responseBody response & ByteString.Lazy.toStrict

  when (statusCode (responseStatus response) /= 200) $
    throwM $ bodyToPlanBException body

  case eitherDecodeStrict body of
    Right tokenInfo ->
      pure tokenInfo
    Left errMsg ->
      throwM $ PlanBIntrospectionDeserialization (Text.pack errMsg) body

bodyToPlanBException
  :: ByteString -> PlanBIntrospectionException
bodyToPlanBException bytes =
  case eitherDecodeStrict bytes of
    Right err ->
      PlanBIntrospectionError err
    Left errMsgStr  ->
      let errMsg = Text.pack errMsgStr
      in PlanBIntrospectionDeserialization errMsg bytes
