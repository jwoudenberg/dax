module Util
  ( request
  , runSandbox
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Dax
import Network.Wai (Request(requestMethod))
import Network.Wai.Test
  ( SRequest(SRequest)
  , SResponse
  , Session
  , defaultRequest
  , runSession
  , setPath
  , srequest
  )

request :: ByteString -> ByteString -> ByteString -> Session SResponse
request method path body =
  srequest $
  SRequest
    (setPath defaultRequest {requestMethod = method} path)
    (fromStrict body)

runSandbox :: API NoEffects -> Session () -> IO ()
runSandbox api assertion = sandbox api >>= runSession assertion
