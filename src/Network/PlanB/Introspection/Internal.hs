{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.PlanB.Introspection.Internal
  ( TokenInfo(..)
  , Conf
  , IntrospectionException(..)
  , Problem(..)
  , new
  , newWithManager
  , newFromEnv
  , newWithBackend
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
new = newWithBackend (backendIO Nothing)

-- | Create a new PlanB introspector using the provided endpoint and
-- manager.
newWithManager :: (MonadThrow m, MonadIO m)
               => Manager
               -> Text
               -> m (TokenIntrospector m)
newWithManager manager = newWithBackend (backendIO (Just manager))

backendIO :: MonadIO m
          => Maybe Manager
          -> Backend m
backendIO maybeManager =
  Backend { backendHttp = httpBackendIO maybeManager
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

httpBackendIO :: MonadIO m
              => Maybe Manager
              -> BackendHttp m
httpBackendIO maybeManager =
  BackendHttp { httpRequestExecute = httpRequestExecuteIO maybeManager }

-- | Convenience function. Create a new PlanB introspector using the
-- provided manager. The PlanB server to use is retrieved from the
-- environment variable @PLANB_INTROSPECTION_ENDPOINT@.
newFromEnv :: (MonadThrow m, MonadIO m)
           => Maybe Manager
           -> m (TokenIntrospector m)
newFromEnv maybeManager = do
  let backend = backendIO maybeManager
      BackendEnv { .. } = backendEnv backend
  endpoint <- envLookup "PLANB_INTROSPECTION_ENDPOINT" >>= \ case
    Just ep -> pure ep
    Nothing -> throwM IntrospectionEndpointMissing
  newWithBackend backend endpoint

-- | Create a new PlanB introspector using the provided backend and endpoint.
newWithBackend
  :: (MonadThrow m, MonadIO m)
  => Backend m
  -> Text
  -> m (TokenIntrospector m)
newWithBackend backend introspectionEndpoint = do
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
  response <- httpRequestExecute request
  let body = responseBody response & ByteString.Lazy.toStrict

  when (statusCode (responseStatus response) /= 200) $
    throwM $ bodyToPlanBException body

  case eitherDecodeStrict body of
    Right tokenInfo ->
      pure tokenInfo
    Left errMsg ->
      throwM $ IntrospectionDeserialization (Text.pack errMsg) body

  where backend = conf & confBackend
        BackendHttp { .. } = backend & backendHttp

bodyToPlanBException
  :: ByteString -> IntrospectionException
bodyToPlanBException bytes =
  case eitherDecodeStrict bytes of
    Right err ->
      IntrospectionError err
    Left errMsgStr  ->
      let errMsg = Text.pack errMsgStr
      in IntrospectionDeserialization errMsg bytes
