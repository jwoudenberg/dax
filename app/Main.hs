{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 5000 =<< application [getKelvin]

getKelvin :: Endpoint
getKelvin =
  endpoint
    (static "centigrade" $ capture "temperature" $ static "kelvin" $ get)
    centigradeToKelvin

centigradeToKelvin :: Float -> Float
centigradeToKelvin = (+) 273.15
