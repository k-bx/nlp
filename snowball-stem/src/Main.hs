{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified NLP.Snowball as SB
import NLP.Snowball (Stemmer)
import qualified Data.List as List

analyze :: Stemmer -> Text -> IO ()
analyze s t = do
  T.putStrLn $ "> Analyzing: " <> t
  let words = T.splitOn ", " t
  stems <- SB.stemsWith s words
  T.putStrLn $ T.concat (List.intersperse ", " stems)

main :: IO ()
main = do
  enStemmer <- SB.newStemmer SB.English
  analyze
    enStemmer
    "truth, truthful, truthfulness, countertruth, untruthful, truthology"
  analyze enStemmer "flaw, flaws, flawed, flawless, flawlessness, flawlessly"
  ruStemmer <- SB.newStemmer SB.Russian
  analyze ruStemmer "лес, лесной, лесник, лесничий, лесничество, пролесье"
  analyze ruStemmer "окно, окошко, подоконник, оконный, окнище"
