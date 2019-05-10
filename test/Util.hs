module Util
  ( request
  , runSandbox
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Dax
import Network.HTTP.Types (RequestHeaders)
import Network.Wai (Request(requestHeaders, requestMethod))
import Network.Wai.Test
  ( SRequest(SRequest)
  , SResponse
  , Session
  , defaultRequest
  , runSession
  , setPath
  , srequest
  )

request ::
     ByteString
  -> ByteString
  -> ByteString
  -> RequestHeaders
  -> Session SResponse
request method path body headers =
  srequest $
  SRequest
    (setPath
       defaultRequest {requestMethod = method, requestHeaders = headers}
       path)
    (fromStrict body)

runSandbox :: API NoEffects -> Session () -> IO ()
runSandbox api assertion = sandbox api >>= runSession assertion
