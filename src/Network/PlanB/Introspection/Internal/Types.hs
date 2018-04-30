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

data BackendConfHttp m = BackendConfHttp
  { httpRequestExecute :: Request -> m (Response LazyByteString)
  }

data BackendConfEnv m = BackendConfEnv
  { envLookup :: Text -> m (Maybe Text)
  }

data BackendConf m = BackendConf
  { backendConfHttp :: BackendConfHttp m
  , backendConfEnv  :: BackendConfEnv m
  }

data Conf m = Conf
  { confIntrospectionRequest :: Request
  , confBackend              :: BackendConf m }

-- | Type for RFC7807 @Problem@ objects.
data PlanBError = PlanBError
  { oauth2Error            :: Text
  , oauth2ErrorDescription :: Maybe Text
  , oauth2ErrorURI         :: Maybe Text
  , oauth2ErrorState       :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON (aesonDrop (length ("oauth2" :: String)) snakeCase) ''PlanBError)

data PlanBIntrospectionException = PlanBIntrospectionDeserialization Text ByteString
                                 | PlanBIntrospectionError PlanBError
                                 | PlanBIntrospectionEndpointMissing
  deriving (Typeable, Show)

instance Exception PlanBIntrospectionException
