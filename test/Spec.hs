{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified ContentDecodingSpec
import qualified ContentEncodingSpec
import qualified EffectfulSpec
import qualified RequestHeaderSpec
import qualified ResponseHeaderSpec
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
    , ResponseHeaderSpec.spec
    , EffectfulSpec.spec
    , docGeneration
    ]

docGeneration :: TestTree
docGeneration = testGroup "Documentation generation" []
