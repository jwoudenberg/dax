{-# LANGUAGE OverloadedStrings #-}

module ContentDecodingSpec
  ( spec
  ) where

import Data.ByteString.Lazy (ByteString)
import Dax
import qualified Dax.Response.Json
import Dax.Types
  ( BodyDecoder(BodyDecoder)
  , Response(Response)
  , ResponseEncoder(ResponseEncoder)
  )
import Network.HTTP.Media.MediaType ((//))
import Network.HTTP.Types.Status as Status
import Network.Wai.Test (assertBody, assertStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util (request, runSandbox)

spec :: TestTree
spec =
  testGroup
    "Content decoding"
    [ testCase
        "Request with known Content-Type is decoding using correct decoder" $
      runSandbox multiDecoderRoute $ do
        response <- request "POST" "/" "" [("Content-Type", "animal/bear")]
        assertStatus 200 response
        assertBody "bears" response
    , testCase
        "Request with known Content-Type is decoding using correct decoder (2)" $
      runSandbox multiDecoderRoute $ do
        response <- request "POST" "/" "" [("Content-Type", "animal/marmot")]
        assertStatus 200 response
        assertBody "marmots" response
    , testCase "Request with no Content-Type is decoded using first decoder" $
      runSandbox multiDecoderRoute $ do
        response <- request "POST" "/" "" []
        assertStatus 200 response
        assertBody "bears" response
    , testCase "Request with unknown Content-Type header fails with 415" $
      runSandbox multiDecoderRoute $ do
        response <- request "POST" "/" "" [("Content-Type", "animal/giraffe")]
        assertStatus 415 response
        assertBody "" response
    , testCase
        "Default json decoders decode request with Content-Type `application/json` correctly" $
      runSandbox jsonRoute $ do
        response <-
          request "POST" "/" "42" [("Content-Type", "application/json")]
        assertStatus 200 response
        assertBody "42" response
    , testCase
        "Default json decoders decode request with Content-Type `application/json; charset=utf-8` correctly" $
      runSandbox jsonRoute $ do
        response <-
          request
            "POST"
            "/"
            "42"
            [("Content-Type", "application/json; charset=utf-8")]
        assertStatus 200 response
        assertBody "42" response
    , testCase
        "Request with body that does not match it's Content-Type fails with 400" $
      runSandbox jsonRoute $ do
        response <-
          request
            "POST"
            "/"
            "This isn't valid JSON."
            [("Content-Type", "application/json; charset=utf-8")]
        assertStatus 400 response
        assertBody "" response
    , testCase
        "Request with body that matches Content-Type but results in parse errors fails with 400" $
      runSandbox jsonRoute $ do
        response <-
          request
            "POST"
            "/"
            "{}"
            [("Content-Type", "application/json; charset=utf-8")]
        assertStatus 400 response
        assertBody "" response
    ]

multiDecoderRoute :: API NoEffects
multiDecoderRoute =
  [endpoint (post [bearDecoder, marmotDecoder] [bodyEncoder]) id]

bearDecoder :: BodyDecoder ByteString
bearDecoder = BodyDecoder (const (Just "bears")) ("animal" // "bear")

marmotDecoder :: BodyDecoder ByteString
marmotDecoder = BodyDecoder (const (Just "marmots")) ("animal" // "marmot")

bodyEncoder :: ResponseEncoder ByteString
bodyEncoder =
  ResponseEncoder (\x -> Response x Status.status200 []) ("text" // "plain")

jsonRoute :: API NoEffects
jsonRoute = [endpoint (post [jsonDecoder] [jsonEncoder]) id]

jsonEncoder :: ResponseEncoder Int
jsonEncoder = Dax.Response.Json.succeeds

jsonDecoder :: BodyDecoder Int
jsonDecoder = Dax.Response.Json.autoBodyDecoder
