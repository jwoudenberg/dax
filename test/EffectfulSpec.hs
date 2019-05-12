{-# LANGUAGE OverloadedStrings #-}

module EffectfulSpec
  ( spec
  ) where

import Dax
import qualified Dax.Response.Json
import Network.Wai.Test (Session, assertBody, assertStatus, runSession)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Util (request)

spec :: TestTree
spec =
  testGroup
    "Effectful APIs"
    [ testCase "Request to effectful API succeeds" $
      runEffectful effectfulRoute $ do
        response <- request "GET" "/" "" []
        assertStatus 200 response
        assertBody "42" response
    ]

effectfulRoute :: API IO
effectfulRoute = [endpoint (get [bodyEncoder]) (pure 42)]

bodyEncoder :: ResponseEncoder Int
bodyEncoder = Dax.Response.Json.succeeds

runEffectful :: API IO -> Session () -> IO ()
runEffectful api assertion = application id api >>= runSession assertion
