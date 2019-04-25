{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dax
  ( Endpoint
  , API
  , Route
  , ParamDecoder
  , endpoint
  , get
  , static
  , capture
  , application
  , autoParamDecoder
  -- Convenience re-exports.
  -- Dax should come with batteries. It won't recommend writing custom JSON
  -- encoders but exports `ToJSON` to support writing generic encoders without
  -- needing to take an explicit dependency on Aeson.
  , Aeson.ToJSON
  -- Similar to above a user shouldn't need to pull in another library to be
  -- able use generics for decoding parameters.
  , Scotty.Parsable
  -- Re-export types from internal module.
  , Dax.Types.ResponseEncoder
  ) where

import "base" Data.Bifunctor (first)
import "bytestring" Data.ByteString.Lazy (ByteString)
import "base" Data.Foldable (traverse_)
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8)
import "text" Data.Text.Lazy (fromStrict, toStrict)
import "base" Data.Traversable (for)
import "this" Dax.Types
import "http-media" Network.HTTP.Media.MediaType (MediaType, (//), (/:))
import "http-types" Network.HTTP.Types (Header, Status)

import qualified "aeson" Data.Aeson as Aeson
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.Lazy as Text.Lazy
import qualified "http-media" Network.HTTP.Media.RenderHeader
import qualified "http-types" Network.HTTP.Types.Status as Status
import qualified "wai" Network.Wai as Wai
import qualified "scotty" Web.Scotty as Scotty

data Route a where
  Get :: ResponseEncoder a -> Route a
  PathSegmentStatic :: Text -> Route b -> Route b
  PathSegmentCapture :: Text -> ParamDecoder a -> Route b -> Route (a -> b)

data Endpoint where
  Endpoint :: Route a -> a -> Endpoint

type API = [Endpoint]

newtype ParamDecoder a = ParamDecoder
  { parse :: Text -> Either Text a
  }

autoParamDecoder :: Scotty.Parsable a => ParamDecoder a
autoParamDecoder =
  ParamDecoder {parse = first toStrict . Scotty.parseParam . fromStrict}

endpoint :: Route a -> a -> Endpoint
endpoint = Endpoint

get :: ResponseEncoder a -> Route a
get = Get

static :: Text -> Route a -> Route a
static = PathSegmentStatic

capture :: Text -> ParamDecoder a -> Route b -> Route (a -> b)
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
    Get encoder -> Scotty.get (toPath path) (respond encoder =<< f)
    (PathSegmentStatic name sub) -> serveEndpoint' (Static name path) sub f
    (PathSegmentCapture name decoder sub) ->
      serveEndpoint' (Capture name path) sub (f <*> decodeParam name decoder)

decodeParam :: Text -> ParamDecoder a -> Scotty.ActionM a
decodeParam name ParamDecoder {parse} = do
  value <- Scotty.param (fromStrict name)
  case parse value of
    Right x -> pure x
    Left _ -> Scotty.next

respond :: ResponseEncoder a -> a -> Scotty.ActionM ()
respond ResponseEncoder {encode, mediaType} x = do
  Scotty.setHeader "Content-Type" (fromStrict $ renderMediaType mediaType)
  let Response {body, status, headers} = encode x
  Scotty.status status
  for headers $ \(name, value) ->
    Scotty.setHeader
      (Text.Lazy.pack $ show name)
      (fromStrict $ decodeUtf8 value)
  Scotty.raw body

renderMediaType :: MediaType -> Text
renderMediaType = decodeUtf8 . Network.HTTP.Media.RenderHeader.renderHeader

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
