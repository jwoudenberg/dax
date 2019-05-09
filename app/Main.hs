{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import "text" Data.Text (Text)
import "dax" Dax

import qualified "dax" Dax.Response.Json as Response.Json
import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  putStrLn (show (documentation api))
  Warp.run 5000 =<< sandbox api

api :: API NoEffects
api = [endpoint toKelvinRoute toKelvin]

newtype Kelvin =
  Kelvin Float
  deriving (ToJSON)

newtype Centigrade =
  Centigrade Float
  deriving (Parsable)

toKelvinRoute :: Route NoEffects (Centigrade -> (Text, Kelvin))
toKelvinRoute =
  static "centigrade" $
  capture centigradeDecoder $
  static "kelvin" $
  constantHeader "OtherThing" "ha" $ setHeader "Thing" id $ get [kelvinEncoder]

kelvinEncoder :: ResponseEncoder Kelvin
kelvinEncoder = Response.Json.succeeds

centigradeDecoder :: ParamDecoder Centigrade
centigradeDecoder = autoParamDecoder

toKelvin :: Centigrade -> (Text, Kelvin)
toKelvin (Centigrade temperature) = ("Ho", Kelvin (temperature + 273.15))
