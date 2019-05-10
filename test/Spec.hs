{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified RoutingSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Dax"
    [ RoutingSpec.spec
    , contentDecoding
    , contentEncoding
    , effectfulApis
    , requestHeaders
    , responseHeaders
    , docGeneration
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
