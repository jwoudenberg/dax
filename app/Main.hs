{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 5000 =<< application [endpoint toKelvinRoute toKelvin]

toKelvinRoute :: Route (Float -> Float)
toKelvinRoute =
  static "centigrade" $
  capture "temperature" floatParamDecoder $ static "kelvin" $ get floatEncoder

floatEncoder :: Encoder Float
floatEncoder = autoJsonEncoder

floatParamDecoder :: ParamDecoder Float
floatParamDecoder = autoParamDecoder

toKelvin :: Float -> Float
toKelvin = (+) 273.15
