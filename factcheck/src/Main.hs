{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens ((&), (<&>))
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Safe
import Text.Groom
import Text.HTML.Scalpel.Core
import Text.Regex.PCRE.Heavy

data Book = Book
  { title :: Text
  , mpubYear :: Maybe Int
  } deriving (Show, Eq)

parseBooks :: Scraper Text [Book]
parseBooks = do
  chroots ("tr" @: ["itemtype" @= "http://schema.org/Book"]) $ do
    title <- chroot ("a" @: [hasClass "bookTitle"]) (text "span")
    pubYearRaw <- text $ "td" @: ["width" @= "100%"]
    let mRes = scan [re|published[^0-9]*([0-9]+)|] pubYearRaw
    let mpubYear = mRes & headMay <&> snd >>= headMay <&> T.unpack >>= readMay
    return Book {..}

main :: IO ()
main = do
  t <- T.readFile "data/goodreads_samharris.html"
  putStrLn $ groom $ scrapeStringLike t parseBooks
