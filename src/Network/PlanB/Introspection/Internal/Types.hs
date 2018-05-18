{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.PlanB.Introspection.Internal.Types where

import           Control.Exception.Safe
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as ByteString.Lazy
import           Data.Set
import           Data.Text                      ( Text )
import           GHC.Generics
import           Network.HTTP.Client

type LazyByteString = ByteString.Lazy.ByteString

-- | A 'TokenIntrospector' can be used for introspecting tokens.
data TokenIntrospector m =
  TokenIntrospector
  { introspectToken :: ByteString -> m TokenInfo -- ^ Introspect the provided token.
  }

-- | Information returned on a successful token introspection.
data TokenInfo =
  TokenInfo { tokenInfoExpiresIn :: Int
            , tokenInfoScope     :: Set Text
            , tokenInfoUid       :: Text
            , tokenInfoRealm     :: Text
            } deriving (Show, Generic)

$(deriveJSON (aesonDrop (length ("tokenInfo" :: String)) snakeCase) ''TokenInfo)

data BackendHttp m = BackendHttp
  { httpRequestExecute :: Request -> m (Response LazyByteString)
  }

data BackendEnv m = BackendEnv
  { envLookup :: Text -> m (Maybe Text)
  }

-- | Type for backends for the PlanB token introspector.
data Backend m = Backend
  { backendHttp :: BackendHttp m
  , backendEnv  :: BackendEnv m
  }

data Conf m = Conf
  { confIntrospectionRequest :: Request
  , confBackend              :: Backend m }

-- | Contains the error response data returned from a PlanB server in case of an introspection error.
data ErrorResponse = ErrorResponse
  { errorResponseError            :: Text
  , errorResponseErrorDescription :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON (aesonDrop (length ("errorResponse" :: String)) snakeCase) ''ErrorResponse)

-- | This type models the error scenarios specific to a token introspection attempt. These can be used
-- as exceptions and are in fact thrown by a PlanB token introspector.
data IntrospectionError = DeserializationFailure Text ByteString
                        | InvalidRequest ErrorResponse
                        | InvalidToken ErrorResponse
                        | Other ErrorResponse
                        | NoEndpoint
  deriving (Typeable, Show, Eq, Generic)

instance Exception IntrospectionError
