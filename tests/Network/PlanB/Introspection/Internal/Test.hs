{-# LANGUAGE OverloadedStrings #-}

module Network.PlanB.Introspection.Internal.Test
  ( planBTokenIntrospectionTests
  ) where

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as ByteString.Lazy
import qualified Data.Text.Encoding               as Text
import           Network.HTTP.Client.Internal
import           Network.HTTP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.PlanB.Introspection
import           Network.PlanB.Introspection.Test


planBTokenIntrospectionTests :: [TestTree]
planBTokenIntrospectionTests =
  [ testGroup "Network.PlanB.Introspection.Internal" $
    [ testCase "Introspect token"
        testIntrospectToken
    ]
  ]

testIntrospectToken :: Assertion
testIntrospectToken = do
  let rspBody = ByteString.Lazy.fromStrict . Text.encodeUtf8 $
        "{ \"access_token\": \"some-token\",    \
        \  \"client_id\":    \"test-suite\",    \
        \  \"cn\":           \"some-username\", \
        \  \"expires_in\":   3591,              \
        \  \"grant_type\":   \"password\",      \
        \  \"realm\":        \"/employees\",    \
        \  \"scope\":        [\"uid\"],         \
        \  \"token_type\":   \"Bearer\",        \
        \  \"uid\":          \"some-user-name\" }"
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
                  }
      token = "some-token"
  (_, testState') <- runTestStack testState $ do
    conf <- makeTestConf
    _info <- introspectToken conf token
    pure ()
  1 @=? length (_testStateHttpRequests testState')

_printTokenInfo :: ByteString -> IO ()
_printTokenInfo token = do
  conf <- newConfIO "https://planb-endpoint"
  tokenInfo <- introspectToken conf token
  print tokenInfo
