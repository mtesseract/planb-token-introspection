{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.PlanB.Introspection.Internal.Test
  ( planBTokenIntrospectionTests
  ) where

import           Control.Exception.Safe
import           Control.Monad
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as ByteString.Lazy
import           Data.Format
import qualified Data.Map                         as Map
import qualified Data.Text.Encoding               as Text
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.PlanB.Introspection
import qualified Network.PlanB.Introspection      as PlanB
import           Network.PlanB.Introspection.Test


planBTokenIntrospectionTests :: [TestTree]
planBTokenIntrospectionTests =
  [ testGroup "Network.PlanB.Introspection.Internal" $
    [ testCase "Introspect token"
        testIntrospectToken
    , testCase "Deserialization failure is converted to exception"
        testDeserializationFailure
    ]
  ]

testDeserializationFailure :: Assertion
testDeserializationFailure = do
  let rspBody = ByteString.Lazy.fromStrict . Text.encodeUtf8 $ "{something broken"
      response = Response { responseStatus    = ok200
                          , responseVersion   = http20
                          , responseHeaders   = []
                          , responseBody      = rspBody
                          , responseCookieJar = CJ []
                          , responseClose'    = ResponseClose (pure ())
                          }
      testState = TestState
                  { _testStateHttpResponse = Just response
                  , _testStateHttpRequests = []
                  , _testStateEnvironment = Map.empty
                  }
      tokenName = "some-token-name"
  Left (PlanB.IntrospectionDeserialization _ _) <- try $ runTestStack testState $ do
    introspector <- makeTestIntrospector
    void $ introspectToken introspector tokenName
  pure ()

testIntrospectToken :: Assertion
testIntrospectToken = do
  let rspBody = ByteString.Lazy.fromStrict . Text.encodeUtf8 $ [fmt|
        { "access_token": "some-token",
          "client_id":    "test-suite",
          "cn":           "some-username",
          "expires_in":   3591,
          "grant_type":   "password",
          "realm":        "/employees",
          "scope":        ["uid"],
          "token_type":   "Bearer",
          "uid":          "some-user-name" }
          |]
      response = Response { responseStatus    = ok200
                          , responseVersion   = http20
                          , responseHeaders   = []
                          , responseBody      = rspBody
                          , responseCookieJar = CJ []
                          , responseClose'    = ResponseClose (pure ())
                          }
      testState = TestState
                  { _testStateHttpResponse = Just response
                  , _testStateHttpRequests = []
                  , _testStateEnvironment = Map.empty
                  }
      token = "some-token"
  (_, testState') <- runTestStack testState $ do
    introspector <- makeTestIntrospector
    _info <- introspectToken introspector token
    pure ()
  1 @=? length (_testStateHttpRequests testState')

_printTokenInfo :: ByteString -> IO ()
_printTokenInfo token = do
  introspector <- PlanB.new "https://planb-endpoint"
  tokenInfo <- PlanB.introspectToken introspector token
  print tokenInfo
