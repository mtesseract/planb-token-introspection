{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.PlanB.Introspection.Internal
  ( TokenInfo(..)
  , Conf
  , PlanBIntrospectionException
  , new
  , newFromEnv
  , newCustom
  , httpRequestExecuteIO
  , introspectToken
  ) where

import           Control.Arrow
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
import qualified System.Environment                         as Env

import           Network.PlanB.Introspection.Internal.Types

-- | Create a new PlanB introspector using the provided endpoint.
new :: (MonadThrow m, MonadIO m)
    => Text
    -> m (TokenIntrospector m)
new endpoint = do
  conf <- newConf backendIO endpoint
  pure $ TokenIntrospector { introspectToken = introspectTokenImpl conf }
backendIO :: MonadIO m => Backend m
backendIO =
  Backend { backendHttp = httpBackendIO
          , backendEnv  = envBackendIO }

envBackendIO :: MonadIO m => BackendEnv m
envBackendIO =
  BackendEnv { envLookup = envLookupIO }

envLookupIO :: MonadIO m => Text -> m (Maybe Text)
envLookupIO =
  Text.unpack
  >>> Env.lookupEnv
  >>> fmap (fmap Text.pack)
  >>> liftIO

httpBackendIO :: MonadIO m => BackendHttp m
httpBackendIO =
  BackendHttp { httpRequestExecute = httpRequestExecuteIO Nothing }

newFromEnv :: (MonadThrow m, MonadIO m)
           => m (TokenIntrospector m)
newFromEnv = do
  let backend    = backendIO
      BackendEnv { .. } = backendEnv backend
  endpoint <- envLookup "PLANB_INTROSPECTION_ENDPOINT" >>= \ case
    Just ep -> pure ep
    Nothing -> throwM PlanBIntrospectionEndpointMissing
  newCustom backend endpoint

newCustom
  :: (MonadThrow m, MonadIO m)
  => Backend m
  -> Text
  -> m (TokenIntrospector m)
newCustom backend introspectionEndpoint = do
  conf <- newConf backend introspectionEndpoint
  pure $ TokenIntrospector { introspectToken = introspectTokenImpl conf }

newConf
  :: MonadThrow m
  => Backend m
  -> Text
  -> m (Conf m)
newConf backend introspectionEndpoint = do
  introspectionRequest <- parseRequest introspectionEndpointStr
  pure Conf { confIntrospectionRequest = introspectionRequest
            , confBackend              = backend }
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

introspectTokenImpl
  :: MonadThrow m
  => Conf m
  -> ByteString
  -> m TokenInfo
introspectTokenImpl conf token = do
  let endpoint    = confIntrospectionRequest conf
      bearerToken = "Bearer " <> token
      request     = endpoint { method         = "GET"
                             , path           = "/oauth2/tokeninfo"
                             , requestHeaders = [("Authorization", bearerToken)] }
      httpBackend = conf
                    & confBackend
                    & backendHttp
  response <- httpRequestExecute httpBackend request
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
