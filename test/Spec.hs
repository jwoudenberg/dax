{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Dax
import qualified Dax.Response.Json
import Network.Wai.Test
  ( Session
  , assertBody
  , assertStatus
  , defaultRequest
  , request
  , runSession
  , setPath
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Dax"
    [ routing
    , contentDecoding
    , contentEncoding
    , effectfulApis
    , requestHeaders
    , responseHeaders
    , docGeneration
    ]

routing :: TestTree
routing =
  testGroup
    "Routing"
    [ testCase "Request to static route with multiple segments succeeds" $
      runSandbox staticRoute $ do
        response <- request $ setPath defaultRequest "/teas/green/lemon"
        assertStatus 200 response
    , testCase "Request to non existing route fails with 404" $
      runSandbox staticRoute $ do
        response <- request $ setPath defaultRequest "/non-existing/route"
        assertStatus 404 response
    , testCase
        "Handler for route with multiple capture segments receives arguments in right order" $
      runSandbox subtractRoute $ do
        response <- request $ setPath defaultRequest "/from/6/subtract/4"
        assertStatus 200 response
        assertBody "2" response
    -- Request with unsupported method fails with 405
    ]

contentDecoding :: TestTree
contentDecoding = testGroup "Content decoding" []

contentEncoding :: TestTree
contentEncoding = testGroup "Content encoding" []

effectfulApis :: TestTree
effectfulApis = testGroup "Effectful APIs" []

requestHeaders :: TestTree
requestHeaders = testGroup "Request headers" []

responseHeaders :: TestTree
responseHeaders = testGroup "Response headers" []

docGeneration :: TestTree
docGeneration = testGroup "Documentation generation" []

runSandbox :: API NoEffects -> Session () -> IO ()
runSandbox api assertion = sandbox api >>= runSession assertion

staticRoute :: API NoEffects
staticRoute =
  [ endpoint
      (static "teas" $ static "green" $ static "lemon" $ get [intEncoder])
      42
  ]

subtractRoute :: API NoEffects
subtractRoute =
  [ endpoint
      (static "from" $
       capture intDecoder $
       static "subtract" $ capture intDecoder $ get [intEncoder])
      (-)
  ]

intEncoder :: ResponseEncoder Int
intEncoder = Dax.Response.Json.succeeds

intDecoder :: ParamDecoder Int
intDecoder = autoParamDecoder
