{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified ContentDecodingSpec
import qualified ContentEncodingSpec
import qualified RoutingSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Dax"
    [ RoutingSpec.spec
    , ContentDecodingSpec.spec
    , ContentEncodingSpec.spec
    , effectfulApis
    , requestHeaders
    , responseHeaders
    , docGeneration
    ]

effectfulApis :: TestTree
effectfulApis = testGroup "Effectful APIs" []

requestHeaders :: TestTree
requestHeaders = testGroup "Request headers" []

responseHeaders :: TestTree
responseHeaders = testGroup "Response headers" []

docGeneration :: TestTree
docGeneration = testGroup "Documentation generation" []
