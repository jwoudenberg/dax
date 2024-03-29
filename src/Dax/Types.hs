{-# LANGUAGE PackageImports #-}

module Dax.Types where

import "bytestring" Data.ByteString.Lazy (ByteString)
import "http-media" Network.HTTP.Media.MediaType (MediaType)
import "http-types" Network.HTTP.Types (Status)

data ResponseEncoder a = ResponseEncoder
  { encode :: a -> Response
  , mediaType :: MediaType
  }

data Response = Response
  { body :: ByteString
  , status :: Status
  }

data BodyDecoder a = BodyDecoder
  { decode :: ByteString -> Maybe a
  , bodyMediaType :: MediaType
  }
