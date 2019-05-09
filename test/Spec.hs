{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Dax
import qualified Dax.Response.Json
import Network.Wai.Test
  ( Session
  , assertStatus
  , defaultRequest
  , request
  , runSession
  , setPath
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main =
  defaultMain $
  testGroup
    "Dax.sandbox"
    [ testCase "Request to static route with multiple segments succeeds" $
      runSandbox staticRoute $ do
        response <- request $ setPath defaultRequest "/teas/green/lemon"
        assertStatus 200 response
    , testCase "Request to non existing route fails with 404" $
      runSandbox staticRoute $ do
        response <- request $ setPath defaultRequest "/non-existing/route"
        assertStatus 404 response
    ]

runSandbox :: API NoEffects -> Session () -> IO ()
runSandbox api assertion = sandbox api >>= runSession assertion

staticRoute :: API NoEffects
staticRoute =
  [ endpoint
      (static "teas" $ static "green" $ static "lemon" $ get [intEncoder])
      42
  ]

intEncoder :: ResponseEncoder Int
intEncoder = Dax.Response.Json.succeeds
