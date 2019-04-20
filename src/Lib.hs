{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import "text" Data.Text (Text)
import "wai" Network.Wai

import qualified "aeson" Data.Aeson as Aeson
import qualified "http-types" Network.HTTP.Types.Status as Status

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Decoder a = Text -> a

param :: Decoder a -> (a -> b) -> Request -> (b, Request)
param = undefined

data Route a where
  Get :: (Aeson.ToJSON b) => Route b
  StaticParam :: Text -> Route b -> Route b
  Param :: Text -> Decoder a -> Route b -> Route (a -> b)

data Endpoint where
  Endpoint :: Route a -> a -> Endpoint

type API = [Endpoint]

-- Naive serve function
serve :: API -> Request -> Response
serve endpoints req =
  case route req endpoints of
    Just endpoint -> serveEndpoint endpoint (RequestInfo (pathInfo req))
    Nothing -> responseLBS Status.status404 [] ""

data RequestInfo = RequestInfo
  { path :: [Text]
  }

route :: Request -> API -> Maybe Endpoint
route = undefined

serveEndpoint :: Endpoint -> RequestInfo -> Response
serveEndpoint (Endpoint route x) req = serveRoute route x req

serveRoute :: Route a -> a -> RequestInfo -> Response
serveRoute Get x _ = responseLBS Status.status200 [] (Aeson.encode x)
serveRoute (StaticParam _ sub) x req =
  serveRoute sub x req {path = tail (path req)}
serveRoute (Param _ decoder sub) f req =
  let newPath = tail (path req)
      param = decoder . head $ path req
   in serveRoute sub (f param) req {path = newPath}
