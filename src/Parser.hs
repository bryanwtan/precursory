{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Text (Text)

newtype StoryId = StoryId {unStoryId :: Int}
  deriving newtype (FromJSON)
  deriving (Show)

newtype TopStories = TopStories {unTopStories :: [StoryId]}
  deriving newtype (FromJSON)

data Story = Story
  { by :: Text
  , descendants :: Maybe Int
  , id :: Int
  , kids :: Maybe [Int]
  , score :: Int
  , time :: Integer
  , title :: Text
  , resource_type :: Text
  , url :: Maybe Text
  }
  deriving (Show)

instance FromJSON Story where
  parseJSON = withObject "Story" $ \o ->
    Story <$> (o .: "by")
      <*> o .:? "descendants"
      <*> o .: "id"
      <*> o .:? "kids"
      <*> o .: "score"
      <*> o .: "time"
      <*> o .: "title"
      <*> pure "story"
      <*> o .:? "url"
