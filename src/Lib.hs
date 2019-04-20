{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import "text" Data.Text (Text)
import "text" Data.Text.Lazy (fromStrict)
import "wai" Network.Wai

import qualified "aeson" Data.Aeson as Aeson
import qualified "http-types" Network.HTTP.Types.Status as Status
import qualified "scotty" Web.Scotty as Scotty

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Route a where
  Get :: (Aeson.ToJSON b) => Route b
  StaticParam :: Text -> Route b -> Route b
  Param :: (Scotty.Parsable a) => Text -> Route b -> Route (a -> b)

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
serveRoute (Param _ sub) f req =
  let newPath = tail (path req)
      param' = Scotty.parseParam . fromStrict . head $ path req
   in case param' of
        Right param -> serveRoute sub (f param) req {path = newPath}
        Left _ -> responseLBS Status.status400 [] ""
