{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)

data Story = Story
    { title :: Text
    , body :: Text
    }
    deriving (Show)

instance FromJSON Story where
    parseJSON = withObject "Story" $ \o ->
        Story
            <$> o .: "incoming_title"
            <*> o .: "incoming_body"

newtype StoryId = StoryId {unStoryId :: Integer}
    deriving newtype (FromJSON)

newtype TopStories = TopStories {unTopStories :: [StoryId]}
    deriving newtype (FromJSON)
