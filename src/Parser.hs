{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)

data Story = Story
    { title :: Text
    , body :: Text
    }
    deriving (Show)

instance FromJSON Story where
    parseJSON (Object obj) = do
        title <- obj .: "incoming_title"
        body <- obj .: "incoming_body"
        return (Story{title = title, body = body})
    parseJSON _ = empty
