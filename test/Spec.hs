{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Dax
import qualified Dax.Response.Json
import Network.Wai (Request(requestMethod))
import Network.Wai.Test
  ( SRequest(SRequest)
  , SResponse
  , Session
  , assertBody
  , assertStatus
  , defaultRequest
  , runSession
  , setPath
  , srequest
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
        response <- request "GET" "/teas/green/lemon" ""
        assertStatus 200 response
    , testCase "Request to non existing route fails with 404" $
      runSandbox staticRoute $ do
        response <- request "GET" "/non-existing/route" ""
        assertStatus 404 response
    , testCase "Request with unsupported method fails with 404" $
      runSandbox staticRoute $ do
        response <- request "POST" "/teas/green/lemon" ""
        -- This should be a 405 error, but Scotty disagrees. We won't be able to
        -- change this until we swap out Scotty.
        assertStatus 404 response
    , testCase
        "Handler for route with multiple capture segments receives arguments in right order" $
      runSandbox subtractRoute $ do
        response <- request "GET" "/from/6/subtract/4" ""
        assertStatus 200 response
        assertBody "2" response
    , testCase "Route can accept query string parameters" $
      runSandbox queryRoute $ do
        response <- request "GET" "/echo?number=4" ""
        assertStatus 200 response
        assertBody "4" response
    , testCase "Request to POST route succeeds" $
      runSandbox postRoute $ do
        response <- request "POST" "/echo" "42"
        assertStatus 200 response
        assertBody "42" response
    , testCase "Request to PUT route succeeds" $
      runSandbox putRoute $ do
        response <- request "PUT" "/echo" "42"
        assertStatus 200 response
        assertBody "42" response
    , testCase "Request to DELETE route succeeds" $
      runSandbox deleteRoute $ do
        response <- request "DELETE" "/echo" ""
        assertStatus 200 response
    ]

request :: ByteString -> ByteString -> ByteString -> Session SResponse
request method path body =
  srequest $
  SRequest
    (setPath defaultRequest {requestMethod = method} path)
    (fromStrict body)

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
      (static "teas" $ static "green" $ static "lemon" $ get [bodyEncoder])
      42
  ]

subtractRoute :: API NoEffects
subtractRoute =
  [ endpoint
      (static "from" $
       capture paramDecoder $
       static "subtract" $ capture paramDecoder $ get [bodyEncoder])
      (-)
  ]

queryRoute :: API NoEffects
queryRoute =
  [ endpoint
      (static "echo" $ query "number" paramDecoder $ get [bodyEncoder])
      (maybe (-1) id)
  ]

postRoute :: API NoEffects
postRoute = [endpoint (static "echo" $ post [bodyDecoder] [bodyEncoder]) id]

putRoute :: API NoEffects
putRoute = [endpoint (static "echo" $ put [bodyDecoder] [bodyEncoder]) id]

deleteRoute :: API NoEffects
deleteRoute = [endpoint (static "echo" $ delete [bodyEncoder]) 42]

bodyEncoder :: ResponseEncoder Int
bodyEncoder = Dax.Response.Json.succeeds

bodyDecoder :: BodyDecoder Int
bodyDecoder = Dax.Response.Json.autoBodyDecoder

paramDecoder :: ParamDecoder Int
paramDecoder = autoParamDecoder
