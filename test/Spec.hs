{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified ContentDecodingSpec
import qualified ContentEncodingSpec
import qualified RequestHeaderSpec
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
    , RequestHeaderSpec.spec
    , effectfulApis
    , responseHeaders
    , docGeneration
    ]

effectfulApis :: TestTree
effectfulApis = testGroup "Effectful APIs" []

responseHeaders :: TestTree
responseHeaders = testGroup "Response headers" []

docGeneration :: TestTree
docGeneration = testGroup "Documentation generation" []
