module Main where

import NLP.POS

main :: IO ()
main = do
  tagger <- defaultTagger
  putStrLn $ tagStr tagger "We can but hope that everything will be fine."
  putStrLn $ tagStr tagger "It's sad but true."
  putStrLn $ tagStr tagger "Jack brings nothing but trouble."
  putStrLn $ tagStr tagger "As we were talking, I realised how to solve the issue."
  putStrLn $ tagStr tagger "This hot dog isn't as big as usual."
  putStrLn $ tagStr tagger "This hot dog isn't as big as usual."
  putStrLn $ tagStr tagger "This hot dog isn't as big as I expected."
  putStrLn $ tagStr tagger "I work as a teacher."
  putStrLn $ tagStr tagger "Let's do it this way!"
  putStrLn $ tagStr tagger "This is way too much!"
  putStrLn $ tagStr tagger "The prices are going down."
  putStrLn $ tagStr tagger "Someone pushed him and he fell down the stairs."
  putStrLn $ tagStr tagger "I’ve been feeling rather down lately."
  putStrLn $ tagStr tagger "It's not easy to down a cup of coffee in one gulp."
  putStrLn $ tagStr tagger "Bring a down jacket and a pair of gloves, and you'll be fine."
