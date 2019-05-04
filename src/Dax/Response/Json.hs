{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dax.Response.Json
  ( succeeds
  , succeedsWithoutResponse
  , mayNotFind
  , mayNotValidate
  , mayFail
  ) where

import "aeson" Data.Aeson ((.=))
import "text" Data.Text.Encoding (decodeUtf8)
import "this" Dax.Types
import "http-media" Network.HTTP.Media.MediaType (MediaType, (//), (/:))
import "http-types" Network.HTTP.Types.Status (Status)

import qualified "aeson" Data.Aeson as Aeson
import qualified "http-types" Network.HTTP.Types.Status as Status

-- |
-- Encode a response for an endpoint that always succeeds in getting a response.
succeeds :: (Aeson.ToJSON a) => ResponseEncoder a
succeeds = ResponseEncoder {encode = okResponse, mediaType = applicationJson}

-- |
-- Encode a response for an endpoint that returns no data.
succeedsWithoutResponse :: ResponseEncoder ()
succeedsWithoutResponse =
  ResponseEncoder
    { encode = const (defaultResponse Status.noContent204)
    , mediaType = applicationJson
    }

-- |
-- Use this for endpoints that may not find the data the user requested. If the
-- endpoint returns `Nothing`, than this encoder will create a 404 response.
mayNotFind :: (Aeson.ToJSON a) => ResponseEncoder (Maybe a)
mayNotFind =
  ResponseEncoder
    { encode = maybe (defaultResponse Status.status404) okResponse
    , mediaType = applicationJson
    }

-- |
-- Use this for endpoints that validate data on the request. If the validation
-- fails it will create a 400 response.
mayNotValidate ::
     (Aeson.ToJSON e, Aeson.ToJSON a) => ResponseEncoder (Either e a)
mayNotValidate =
  ResponseEncoder
    { encode = either (customResponse (const Status.status400)) okResponse
    , mediaType = applicationJson
    }

-- |
-- This is the most general response encoder. You should use it if you need to
-- create errors with custom status codes.
mayFail ::
     (Aeson.ToJSON e, Aeson.ToJSON a)
  => (e -> Status)
  -> ResponseEncoder (Either e a)
mayFail withStatus =
  ResponseEncoder
    { encode = either (customResponse withStatus) okResponse
    , mediaType = applicationJson
    }

okResponse :: (Aeson.ToJSON a) => a -> Response
okResponse = customResponse (const Status.status200)

customResponse :: (Aeson.ToJSON a) => (a -> Status) -> a -> Response
customResponse withStatus a = Response (Aeson.encode a) (withStatus a) []

defaultResponse :: Status -> Response
defaultResponse status' = Response (Aeson.encode body') status' []
  where
    body' :: Aeson.Object
    body' = "error" .= decodeUtf8 (Status.statusMessage status')

applicationJson :: MediaType
applicationJson = "application" // "json" /: ("charset", "utf-8")
