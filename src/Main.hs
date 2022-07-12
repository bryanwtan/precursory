{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (decode)
import Data.Text (unpack)
import Network.HTTP.Simple (Response, getResponseBody)
import Parser (Story, StoryId, TopStories, title, unStoryId, unTopStories)
import Request (httpGet)

topStoriesUrl = "https://hacker-news.firebaseio.com/v0/topstories.json"

buildStoryUrl :: StoryId -> String
-- semigroup will still work for Text where ++ will not (not a list)
buildStoryUrl storyId = "https://hacker-news.firebaseio.com/v0/item/" <> show (unStoryId storyId) <> ".json?print=pretty"

buildStoryUrls :: TopStories -> [String]
-- eta reduction to achieve pointfree style
-- keep on eye on readability
-- buildStoryUrls topStories = map buildStoryUrl (unTopStories topStories)
-- buildStoryUrls topStories = map buildStoryUrl $ unTopStories topStories
buildStoryUrls = map buildStoryUrl . unTopStories

-- T.pack [Char] -> Text and T.unpack

processStory storyCandidate = getResponseBody storyCandidate :: Story

-- can think about types and set to undefined
-- countWordsInAllStories :: String -> [Story] -> Int
-- countWordsInAllStories word stories = undefined

-- sometimes the compiler can figure out what it expects here
-- countWordsInAllStories :: _

-- counts the number of stories of top stories had the word
countWordsInAllStories :: String -> [Story] -> Int
countWordsInAllStories word stories = length $ filter hasWord stories
  where
    hasWord = elem word . words . unpack . title

-- think about code in terms of side effects
main :: IO ()
main = do
  response <- httpGet topStoriesUrl :: IO (Response TopStories)
  let topStories = getResponseBody response
  -- topStories <- getResponseBody <$> httpGet topStoriesUrl :: IO (TopStories)
  --
  let storyUrls = buildStoryUrls topStories
  allStories <- mapConcurrently httpGet storyUrls
  let processedStories = map processStory allStories
  -- allStories <- mapConcurrently (fmap processStory . httpGet . buildStoryUrl) (unTopStories topStories)
  --
  -- missing step to use record field for Story
  let instanceCount :: Int = countWordsInAllStories "a" processedStories
  print instanceCount
