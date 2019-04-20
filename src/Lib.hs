{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import "base" Data.Foldable (traverse_)
import "text" Data.Text (Text)
import "text" Data.Text.Lazy (fromStrict)
import "scotty" Web.Scotty

import qualified "aeson" Data.Aeson as Aeson
import qualified "text" Data.Text as Text
import qualified "http-types" Network.HTTP.Types.Status as Status
import qualified "wai" Network.Wai as Wai

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Route a where
  Get :: (Aeson.ToJSON b) => Route b
  PathSegmentStatic :: Text -> Route b -> Route b
  PathSegmentCapture :: (Parsable a) => Text -> Route b -> Route (a -> b)

data Endpoint where
  Endpoint :: Route a -> a -> Endpoint

type API = [Endpoint]

-- |
-- Interpret the API as a WAI application.
application :: API -> IO Wai.Application
application = scottyApp . traverse_ serveEndpoint

serveEndpoint :: Endpoint -> ScottyM ()
serveEndpoint (Endpoint route handler) = serveEndpoint' End route (pure handler)

serveEndpoint' :: Path -> Route a -> ActionM a -> ScottyM ()
serveEndpoint' path route f =
  case route of
    Get -> get (toPath path) (json =<< f)
    (PathSegmentStatic name sub) -> serveEndpoint' (Static name path) sub f
    (PathSegmentCapture name sub) ->
      serveEndpoint' (Capture name path) sub (f <*> param (fromStrict name))

data Path
  = End
  | Static Text
           Path
  | Capture Text
            Path

toPath :: Path -> RoutePattern
toPath = capture . Text.unpack . toPath' ""

toPath' :: Text -> Path -> Text
toPath' acc End = acc
toPath' acc (Static segment rest) = toPath' ("/" <> segment <> acc) rest
toPath' acc (Capture name rest) = toPath' ("/:" <> name <> acc) rest
