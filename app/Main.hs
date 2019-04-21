{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import qualified "warp" Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.run 5000 =<< application [getKelvin]

getKelvin :: Endpoint
getKelvin =
  Endpoint
    (PathSegmentStatic "centigrade" .
     PathSegmentCapture "temperature" . PathSegmentStatic "kelvin" $
     Get)
    centigradeToKelvin

centigradeToKelvin :: Float -> Float
centigradeToKelvin = (+) 273.15
