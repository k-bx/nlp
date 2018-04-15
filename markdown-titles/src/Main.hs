{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isUpper)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NLP.Tokenize.Text

data Category
  = Title
  | Paragraph
  deriving (Eq, Show)

bigEnough :: Int
bigEnough = 80

allCaps :: T.Text -> Bool
allCaps t = all isCap (T.words t)
  where
    isCap x =
      case T.uncons x of
        Nothing -> False
        Just (h, _) -> isUpper h

noEndingDot :: T.Text -> Bool
noEndingDot t =
  if T.takeEnd 1 t `elem` [".", "?", "!"]
    then False
    else True

categorize :: T.Text -> Category
categorize t = Paragraph
  -- if T.length t > bigEnough
  --   then Paragraph
  --   else case tokenize t of
  --          [_] -> Title
  --          _ ->
  --            if allCaps t
  --              then Title
  --              else if noEndingDot t
  --                     then Title
  --                     else Paragraph

main :: IO ()
main = do
  headings <- T.lines <$> T.readFile "data/headings.txt"
  textpars <- T.lines <$> T.readFile "data/textpars.txt"
  putStrLn $ "headings length:"
  print $ length headings
  putStrLn $ "textpars:"
  print $ length textpars
  let headingsTestNum = round (fromIntegral (length headings) * 0.8)
  let headingsTest = take headingsTestNum headings
  let headingsValidate = drop headingsTestNum headings
  let parsTestNum = round (fromIntegral (length textpars) * 0.8)
  let parsTest = take parsTestNum textpars
  let parsValidate = drop parsTestNum textpars
  let headingsTestCategorized = map categorize headingsTest
  let parsTestCategorized = map categorize parsTest
  let headingsCorrect = (filter (== Title)) headingsTestCategorized
  let headingsIncorrect = (filter (/= Title)) headingsTestCategorized
  let parsCorrect = (filter (== Paragraph)) parsTestCategorized
  let parsIncorrect = (filter (/= Paragraph)) parsTestCategorized
  putStrLn $ "Correctly stated headings: "
  print $ length headingsCorrect
  putStrLn $ "Incorrectly stated headings: "
  print $ length headingsIncorrect
  putStrLn $ "Correctly stated pars: "
  print $ length parsCorrect
  putStrLn $ "Incorrectly stated pars: "
  print $ length parsIncorrect
  let precision =
        (fromIntegral $ length headingsCorrect) /
        (fromIntegral $ length headingsCorrect + length parsIncorrect)
  putStrLn $ "Precision: "
  print precision
  let recall =
        (fromIntegral $ length headingsCorrect) /
        ((fromIntegral $ length headingsCorrect) +
         (fromIntegral $ length headingsIncorrect))
  putStrLn $ "Recall: "
  print recall
  let f1 = 2 * (precision * recall) / (precision + recall)
  putStrLn $ "F1:"
  print f1
  putStrLn $ "Accuracy:"
  print $
    (fromIntegral $ length headingsCorrect + length parsCorrect) /
    (fromIntegral $ length headings + length textpars)
  return ()
