{-# LANGUAGE OverloadedStrings #-}

module RequestHeaderSpec
  ( spec
  ) where

import Dax
import qualified Dax.Response.Json
import Network.Wai.Test (assertBody, assertStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util (request, runSandbox)

spec :: TestTree
spec =
  testGroup
    "Request headers"
    [ testCase "Route that accepts header correctly decodes it" $
      runSandbox headerRoute $ do
        response <- request "GET" "/" "" [("Bear-Count", "12")]
        assertStatus 200 response
        assertBody "12" response
    , testCase "Request with missing header passed `Nothing` to handler" $
      runSandbox headerRoute $ do
        response <- request "GET" "/" "" [("Unrelated-Header", "12")]
        assertStatus 200 response
        assertBody "0" response
    , testCase "Request with unparsable headers fails with 400" $
      runSandbox headerRoute $ do
        response <- request "GET" "/" "" [("Bear-Count", "not an Int")]
        assertStatus 400 response
        assertBody "" response
    ]

headerRoute :: API NoEffects
headerRoute =
  [endpoint (header "Bear-Count" paramDecoder $ get [bodyEncoder]) (maybe 0 id)]

paramDecoder :: ParamDecoder Int
paramDecoder = autoParamDecoder

bodyEncoder :: ResponseEncoder Int
bodyEncoder = Dax.Response.Json.succeeds
