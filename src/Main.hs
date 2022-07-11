module Main where

import Network.HTTP.Simple (Response, getResponseBody)

import Parser (Story)
import Request (httpGet)

main :: IO ()
main = do
    response <- httpGet "https://hacker-news.firebaseio.com/v0/topstories.json" :: IO (Response [Int])
    print (getResponseBody response)
