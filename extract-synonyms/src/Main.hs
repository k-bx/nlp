{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Trans.Resource
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Default (def)
import Data.Hashable
import Data.Maybe
import Data.Semigroup
import Data.String (IsString(fromString))
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.XML.Types (Event)
import GHC.Generics (Generic)
import Text.Pandoc
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Readers.MediaWiki (readMediaWiki)
import Text.XML.Stream.Parse

instance Hashable Pandoc where
  hashWithSalt s x = hashWithSalt s (show x)

data Page = Page
  { title :: Text
  , textPandoc :: Maybe Pandoc
  } deriving (Show, Eq, Generic, Hashable)

data MediaWiki = MediaWiki
  { pages :: [Page]
  } deriving (Show, Eq, Generic, Hashable)

-- | prefix with a namespace
p :: IsString a => [Char] -> a
p x = fromString ("{http://www.mediawiki.org/xml/export-0.10/}" ++ x)

parsePage :: MonadThrow m => ConduitT Event o m (Maybe Page)
parsePage = do
  tagNoAttr (p "page") $ do
    title <- fmap (fromMaybe "") $ tagNoAttr (p "title") content
    _ <- ignoreTreeContent (p "ns")
    _ <- ignoreTreeContent (p "id")
    _ <- ignoreTreeContent (p "redirect")
    text <-
      fmap (fromMaybe "" . fromMaybe Nothing) $
      tagNoAttr (p "revision") $ do
        _ <- ignoreTreeContent (p "id")
        _ <- ignoreTreeContent (p "parentid")
        _ <- ignoreTreeContent (p "timestamp")
        _ <- ignoreTreeContent (p "contributor")
        _ <- ignoreTreeContent (p "minor")
        _ <- ignoreTreeContent (p "comment")
        _ <- ignoreTreeContent (p "model")
        _ <- ignoreTreeContent (p "format")
        t <- tag' (p "text") ignoreAttrs $ \_ -> content
        _ <- ignoreTreeContent (p "sha1")
        return t
    case runPure (readMediaWiki def text) of
      Left pe -> fail ("Error converting to pandoc: " <> show pe)
      Right pd ->
        let textPandoc = Just pd
        in return Page {..}

parseMediaWiki :: MonadThrow m => ConduitT Event o m (Maybe MediaWiki)
parseMediaWiki = do
  tag' (p "mediawiki") ignoreAttrs $ \_ -> do
    _ <- ignoreTreeContent (p "siteinfo")
    pages <- many parsePage
    return (MediaWiki pages)

main :: IO ()
main = do
  let fpath =
        "/home/kb/Downloads/wiktionary/uk/20180301/ukwiktionary-20180301-pages-articles.xml/data"
  -- let fpath =
  --       "/home/kb/Downloads/wiktionary/uk/20180301/ukwiktionary-20180301-pages-articles.xml/datasubset.xml"
  mediaWiki <-
    runResourceT $
    runConduit $
    parseFile def fpath .| force "mediawiki required" parseMediaWiki
  let el = last (pages mediaWiki)
  S.putStrLn ("> Title: " <> title el)
  S.putStrLn
    ("> Text: " <>
     (T.unlines (take 10 (T.lines (T.pack (maybe "" show (textPandoc el)))))))
