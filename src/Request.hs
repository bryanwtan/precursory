{-# LANGUAGE OverloadedStrings #-}

module Request where

-- data and type constructors (..)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Simple (
    Request (..),
    Response,
    httpJSON,
 )

get url = do
    initReq <- parseRequest url
    let req =
            initReq
                { method = "GET"
                }
    httpJSON req
