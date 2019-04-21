{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Endpoint
  , API
  , Route
  , endpoint
  , get
  , static
  , capture
  , application
  ) where

import "base" Data.Foldable (traverse_)
import "text" Data.Text (Text)
import "text" Data.Text.Lazy (fromStrict)

import qualified "aeson" Data.Aeson as Aeson
import qualified "text" Data.Text as Text
import qualified "http-types" Network.HTTP.Types.Status as Status
import qualified "wai" Network.Wai as Wai
import qualified "scotty" Web.Scotty as Scotty

data Route a where
  Get :: (Aeson.ToJSON b) => Route b
  PathSegmentStatic :: Text -> Route b -> Route b
  PathSegmentCapture :: (Scotty.Parsable a) => Text -> Route b -> Route (a -> b)

data Endpoint where
  Endpoint :: Route a -> a -> Endpoint

type API = [Endpoint]

endpoint :: Route a -> a -> Endpoint
endpoint = Endpoint

get :: (Aeson.ToJSON a) => Route a
get = Get

static :: Text -> Route a -> Route a
static = PathSegmentStatic

capture :: (Scotty.Parsable a) => Text -> Route b -> Route (a -> b)
capture = PathSegmentCapture

-- |
-- Interpret the API as a WAI application.
application :: API -> IO Wai.Application
application = Scotty.scottyApp . traverse_ serveEndpoint

serveEndpoint :: Endpoint -> Scotty.ScottyM ()
serveEndpoint (Endpoint route handler) = serveEndpoint' End route (pure handler)

serveEndpoint' :: Path -> Route a -> Scotty.ActionM a -> Scotty.ScottyM ()
serveEndpoint' path route f =
  case route of
    Get -> Scotty.get (toPath path) (Scotty.json =<< f)
    (PathSegmentStatic name sub) -> serveEndpoint' (Static name path) sub f
    (PathSegmentCapture name sub) ->
      serveEndpoint'
        (Capture name path)
        sub
        (f <*> Scotty.param (fromStrict name))

data Path
  = End
  | Static Text
           Path
  | Capture Text
            Path

toPath :: Path -> Scotty.RoutePattern
toPath = Scotty.capture . Text.unpack . toPath' ""

toPath' :: Text -> Path -> Text
toPath' acc End = acc
toPath' acc (Static segment rest) = toPath' ("/" <> segment <> acc) rest
toPath' acc (Capture name rest) = toPath' ("/:" <> name <> acc) rest
