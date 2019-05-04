{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import "dax" Dax

import qualified "dax" Dax.Response.Json as Response.Json
import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  putStrLn (show (documentation api))
  Warp.run 5000 =<< ioApplication api

api :: API IO
api = [endpoint toKelvinRoute toKelvin]

newtype Kelvin =
  Kelvin Float
  deriving (ToJSON)

newtype Centigrade =
  Centigrade Float
  deriving (Parsable)

toKelvinRoute :: Route IO (Centigrade -> IO Kelvin)
toKelvinRoute =
  static "centigrade" $
  capture "temperature" centigradeDecoder $ static "kelvin" $ get kelvinEncoder

kelvinEncoder :: ResponseEncoder Kelvin
kelvinEncoder = Response.Json.succeeds

centigradeDecoder :: ParamDecoder Centigrade
centigradeDecoder = autoParamDecoder

toKelvin :: Centigrade -> IO Kelvin
toKelvin (Centigrade temperature) = pure $ Kelvin (temperature + 273.15)
