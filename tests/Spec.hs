{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty

import           Network.PlanB.Introspection.Internal.Test

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: TestTree
tests =
  testGroup "PlanB Token Introspection Test Suite"
  planBTokenIntrospectionTests
