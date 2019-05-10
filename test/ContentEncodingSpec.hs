{-# LANGUAGE OverloadedStrings #-}

module ContentEncodingSpec
  ( spec
  ) where

import Data.ByteString.Lazy (ByteString)
import Dax
import qualified Dax.Response.Json
import Dax.Types (Response(Response), ResponseEncoder(ResponseEncoder))
import Network.HTTP.Media.MediaType ((//))
import Network.HTTP.Types.Status as Status
import Network.Wai.Test (assertBody, assertHeader, assertStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util (request, runSandbox)

spec :: TestTree
spec =
  testGroup
    "Content encoding"
    [ testCase
        "Response to request with known Accept header is using correct encoder" $
      runSandbox multiEncoderRoute $ do
        response <-
          request "GET" "/" "" [("Accept", "animal/giraffe, animal/bear")]
        assertStatus 200 response
        assertBody "bears" response
    , testCase
        "Response to request with known Accept header is using correct encoder (2)" $
      runSandbox multiEncoderRoute $ do
        response <-
          request "GET" "/" "" [("Accept", "animal/giraffe, animal/marmot")]
        assertStatus 200 response
        assertBody "marmots" response
    , testCase "Request with unknown Accept header fails with 406" $
      runSandbox multiEncoderRoute $ do
        response <- request "GET" "/" "" [("Accept", "animal/giraffe")]
        assertStatus 406 response
        assertBody "" response
    , testCase
        "Response to request with known Accept header is using correct encoder" $
      runSandbox multiEncoderRoute $ do
        response <- request "GET" "/" "" [("Accept", "animal/bear")]
        assertStatus 200 response
        assertHeader "Content-Type" "animal/bear" response
    , testCase
        "Response to request with no Accept header is encoded with first encoder" $
      runSandbox multiEncoderRoute $ do
        response <- request "GET" "/" "" []
        assertStatus 200 response
        assertBody "bears" response
    , testCase
        "Response to request with known Accept header is using correct encoder" $
      runSandbox jsonRoute $ do
        response <- request "GET" "/" "" []
        assertStatus 200 response
        assertBody "42" response
        assertHeader "Content-Type" "application/json" response
    ]

multiEncoderRoute :: API NoEffects
multiEncoderRoute = [endpoint (get [bearEncoder, marmotEncoder]) ""]

jsonRoute :: API NoEffects
jsonRoute = [endpoint (get [jsonEncoder]) 42]

jsonEncoder :: ResponseEncoder Int
jsonEncoder = Dax.Response.Json.succeeds

bearEncoder :: ResponseEncoder ByteString
bearEncoder =
  ResponseEncoder
    (\_ -> Response "bears" Status.status200 [])
    ("animal" // "bear")

marmotEncoder :: ResponseEncoder ByteString
marmotEncoder =
  ResponseEncoder
    (\_ -> Response "marmots" Status.status200 [])
    ("animal" // "marmot")
