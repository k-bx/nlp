{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (forM_)
import Data.FileEmbed (embedFile)
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.Scalpel.Core
import Text.RE.TDFA.Text

threadExample :: Text
threadExample = S.toText $(embedFile "data/thread_example.html")

data Post = Post
  { postNick :: Text
  , postContent :: Text
  } deriving (Eq, Show)

data Thread = Thread
  { title :: Text
  , posts :: [Post]
  } deriving (Eq, Show)

cleanUp :: Text -> Text
cleanUp = removeEmptyNewlines . removeAds
  where
    removeEmptyNewlines = T.unlines . filter (/= "") . map T.strip . T.lines
    removeAds t = t *=~/ [edBI|р е к л а м а.*-->///|]

parsePost :: Scraper Text Post
parsePost = do
  postNick <- text $ "a" @: [hasClass "username"]
  c <- text $ "div" @: [hasClass "messageContent"]
  let postContent = cleanUp c
  return Post {..}

parseThread :: Scraper Text Thread
parseThread = do
  title <- text "title"
  posts <- chroots ("li" @: [hasClass "message"]) parsePost
  return Thread {..}

main :: IO ()
main = do
  let mt = scrapeStringLike threadExample parseThread
  case mt of
    Nothing -> putStrLn "Couldn't parse a thread"
    Just t -> do
      S.putStrLn (title t)
      putStrLn ""
      forM_ (posts t) $ \Post {..} -> do
        S.putStrLn postNick
        putStrLn ""
        S.putStrLn postContent
        putStrLn ""
