{-# LANGUAGE OverloadedStrings #-}

module ResponseHeaderSpec
  ( spec
  ) where

import Dax
import qualified Dax.Response.Json
import Network.Wai.Test (assertHeader, assertStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util (request, runSandbox)

spec :: TestTree
spec =
  testGroup
    "Response headers"
    [ testCase "Response header set by handler is encoded on response" $
      runSandbox dynamicHeaderRoute $ do
        response <- request "GET" "/" "" []
        assertStatus 200 response
        assertHeader "Bear-Count" "12" response
    , testCase "Response header set by handler is encoded on response" $
      runSandbox constantHeaderRoute $ do
        response <- request "GET" "/" "" []
        assertStatus 200 response
        assertHeader "Bear-Count" "12" response
    ]

dynamicHeaderRoute :: API NoEffects
dynamicHeaderRoute =
  [endpoint (setHeader "Bear-Count" id $ get [bodyEncoder]) ("12", 0)]

constantHeaderRoute :: API NoEffects
constantHeaderRoute =
  [endpoint (constantHeader "Bear-Count" "12" $ get [bodyEncoder]) 0]

bodyEncoder :: ResponseEncoder Int
bodyEncoder = Dax.Response.Json.succeeds
