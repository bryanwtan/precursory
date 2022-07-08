module Main where

import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml as Yaml
import Network.HTTP.Simple (getResponseBody)

import Request (get)

main :: IO ()
main = do
    response <- get "https://httpbin.org/get"
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
