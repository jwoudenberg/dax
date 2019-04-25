{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Dax.Response.Json
  ( succeeds
  ) where

import "this" Dax.Types
import "http-media" Network.HTTP.Media.MediaType ((//), (/:))

import qualified "aeson" Data.Aeson as Aeson
import qualified "http-types" Network.HTTP.Types.Status as Status

succeeds :: (Aeson.ToJSON a) => ResponseEncoder a
succeeds =
  ResponseEncoder
    { encode = \x -> Response (Aeson.encode x) Status.status200 []
    , mediaType = "application" // "json" /: ("charset", "utf-8")
    }
-- mayNotFind :: (ToJSON a) => ResponseEncoder (Maybe a)
-- mayNotValidate :: (ToJSON e, ToJSON a) => ResponseEncoder (Either e a)
-- mayFail :: (ToJSON e, ToJSON a) => (e -> Status) -> ResponseEncoder (Either e a)
