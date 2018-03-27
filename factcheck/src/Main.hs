{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens hiding (re)
import Data.Aeson.Lens
import Data.Default
import Data.Maybe
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Text (Text)
import Text.Groom
import qualified Data.Text.IO as T
import NLP.CoreNLP
import qualified Network.Wreq as W
import Safe
import System.Environment
import Text.HTML.Scalpel.Core
import qualified Text.HTML.Scalpel.Core as SC
import Text.Pandoc
import Text.Regex.PCRE.Heavy

data Book = Book
  { title :: Text
  , mpubYear :: Maybe Int
  } deriving (Show, Eq)

parseBooks :: Scraper Text [Book]
parseBooks = do
  chroots ("tr" @: ["itemtype" @= "http://schema.org/Book"]) $ do
    title <- chroot ("a" @: [hasClass "bookTitle"]) (SC.text "span")
    pubYearRaw <- SC.text $ "td" @: ["width" @= "100%"]
    let mRes = scan [re|published[^0-9]*([0-9]+)|] pubYearRaw
    let mpubYear = mRes & headMay <&> snd >>= headMay <&> T.unpack >>= readMay
    return Book {..}

getWikiBody :: Text -> Maybe Text
getWikiBody r =
  r ^? key "query" . key "pages" . key "2400008" . key "revisions" . nth 0 .
  key "*" .
  _String

downloadGoodreads :: IO Text
downloadGoodreads = do
  res <- W.get "https://www.goodreads.com/author/list/16593.Sam_Harris"
  return (res ^. W.responseBody & S.toText)

downloadWikiBody :: IO Text
downloadWikiBody = do
  r <-
    W.get
      "https://en.wikipedia.org/w/api.php?action=query&titles=Sam_Harris&prop=revisions&rvprop=content&format=json"
  return $ fromMaybe "" $ getWikiBody (r ^. W.responseBody . to S.toText)

factCheck :: [ParsedDocument] -> [Book] -> IO ()
factCheck = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [corenlp, cache] -> do
      t <- T.readFile "data/goodreads_samharris.html"
      --t <- downloadGoodreads
      let books = scrapeStringLike t parseBooks
      -- mwikiBody <- downloadWikiBody
      mwikiBody <- getWikiBody <$> T.readFile "data/wiki_api_response.json"
      let wikiBody = fromMaybe (error "failed to get wiki body") mwikiBody
      let wikiTxt =
            (either
               (error . show)
               Prelude.id
               (runPure
                  (do mr <- readMediaWiki def wikiBody
                      writePlain def {writerWrapText = WrapNone} mr)))
      docs <-
        launchCoreNLP
          corenlp
          def {numWorkers = 1, cacheDb = Just cache}
          (T.lines wikiTxt)
      putStrLn $ groom $ docs
      factCheck docs (fromMaybe [] books)
      return ()
    _ ->
      putStrLn "Usage: stack exec factcheck -- /path/to/corenlp /path/to/cache"
