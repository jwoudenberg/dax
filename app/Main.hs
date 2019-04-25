{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dax

import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 5000 =<< application [endpoint toKelvinRoute toKelvin]

newtype Kelvin =
  Kelvin Float
  deriving (ToJSON)

newtype Centigrade =
  Centigrade Float
  deriving (Parsable)

toKelvinRoute :: Route (Centigrade -> Kelvin)
toKelvinRoute =
  static "centigrade" $
  capture "temperature" centigradeDecoder $ static "kelvin" $ get kelvinEncoder

kelvinEncoder :: ResponseEncoder Kelvin
kelvinEncoder = json200Encoder

centigradeDecoder :: ParamDecoder Centigrade
centigradeDecoder = autoParamDecoder

toKelvin :: Centigrade -> Kelvin
toKelvin (Centigrade temperature) = Kelvin (temperature + 273.15)
