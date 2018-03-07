{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

data Costs = Costs
  { delCost :: Int
  , insCost :: Int
  , editCost :: Int
  }

editDistance :: Costs -> Text -> Text -> Int
editDistance costs (T.uncons -> Nothing) y@(T.uncons -> _) =
  (delCost costs) * T.length y
editDistance costs x@(T.uncons -> _) (T.uncons -> Nothing) =
  (insCost costs) * T.length x
editDistance costs x@(T.uncons -> Just (ch1, xs1)) y@(T.uncons -> Just (ch2, xs2)) =
  minimum
    [ editDistance costs xs1 y + insCost costs
    , editDistance costs xs2 x + delCost costs
    , editDistance costs xs1 xs2 +
      if ch1 == ch2
        then 0
        else editCost costs
    ]
editDistance _ _ _ = error "impossible!"

main :: IO ()
main = do
  print $ editDistance (Costs 1 1 2) "intention" "execution"
  print $ editDistance (Costs 1 1 1) "leda" "deal"
  print $ editDistance (Costs 1 1 2) "drive" "brief"
  print $ editDistance (Costs 1 1 2) "drive" "drivers"
