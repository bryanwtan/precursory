{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple (getResponseStatusCode)
import Request (get)

main :: IO ()
main = do
    response <- get "GET https://google.com"
    putStrLn $
        "The status code was: "
            ++ show (getResponseStatusCode response)
