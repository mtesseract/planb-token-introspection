{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.PlanB.Introspection.Internal.Types where

import           Control.Exception.Safe
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import           Data.Set
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Client

type LazyByteString = ByteString.Lazy.ByteString

data TokenIntrospector m =
  TokenIntrospector { introspectToken :: ByteString -> m TokenInfo }

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

data Backend m = Backend
  { backendHttp :: BackendHttp m
  , backendEnv  :: BackendEnv m
  }

data Conf m = Conf
  { confIntrospectionRequest :: Request
  , confBackend              :: Backend m }

-- | Type for RFC7807 @Problem@ objects.
data PlanBError = PlanBError
  { oauth2Error            :: Text
  , oauth2ErrorDescription :: Maybe Text
  , oauth2ErrorURI         :: Maybe Text
  , oauth2ErrorState       :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON (aesonDrop (length ("oauth2" :: String)) snakeCase) ''PlanBError)

data IntrospectionException = IntrospectionDeserialization Text ByteString
                            | IntrospectionError PlanBError
                            | IntrospectionEndpointMissing
  deriving (Typeable, Show)

instance Exception IntrospectionException
