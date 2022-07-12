{-# LANGUAGE OverloadedStrings #-}

module Request where

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Simple (httpJSON)

httpGet url = do
  initReq <- parseRequest url
  let req =
        initReq
          { method = "GET"
          }
  httpJSON req
