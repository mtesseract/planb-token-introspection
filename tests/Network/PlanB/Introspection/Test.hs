{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Network.PlanB.Introspection.Test where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Catch                        hiding (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy                       as ByteString.Lazy
import           Data.IORef
import           Network.HTTP.Client

import           Network.PlanB.Introspection.Internal
import           Network.PlanB.Introspection.Internal.Types

runTestStack :: TestState -> TestStack a -> IO (a, TestState)
runTestStack testState m = do
  s <- newIORef testState
  a <- m & (_runTestStack >>> flip runReaderT s)
  (a,) <$> readIORef s

evalTestStack :: TestState -> TestStack a -> IO a
evalTestStack testState m = do
  s <- newIORef testState
  m & (_runTestStack >>> flip runReaderT s)

newtype TestStack a = TestStack
  { _runTestStack :: ReaderT (IORef TestState) IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadReader (IORef TestState)
             , MonadIO
             )

instance MonadUnliftIO TestStack where
  askUnliftIO = do
    (UnliftIO u) <- TestStack askUnliftIO
    pure $ UnliftIO (\ (TestStack m) -> u m)

data TestState =
  TestState { _testStateHttpRequests :: [Request]
            , _testStateHttpResponse :: Maybe (Response ByteString.Lazy.ByteString)
            }

makeFieldsNoPrefix ''TestState

instance MonadState TestState TestStack where
  get = do
    envRef <- ask
    liftIO $ readIORef envRef
  put s = do
    envRef <- ask
    liftIO $ writeIORef envRef s

mockHttpRequestExecute :: Request -> TestStack (Response LazyByteString)
mockHttpRequestExecute request = do
  testStateHttpRequests %= (request :)
  maybeResponse <- gets (view testStateHttpResponse)
  case maybeResponse of
    Just response ->
      pure response
    Nothing ->
      error "FIXME"

makeTestConf :: TestStack (Conf TestStack)
makeTestConf = newConf mockHttpRequestExecute "https://localhost"
