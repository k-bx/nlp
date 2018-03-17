{-# LANGUAGE OverloadedStrings #-}
{-# LAGNUAGE QuasiQuotes #-}

module Main where

import Text.RawString.QQ


example = [r|
<CATALOG>
<PLANT>
<COMMON>Bloodroot</COMMON>
<BOTANICAL>Sanguinaria canadensis</BOTANICAL>
<ZONE>4</ZONE>
<LIGHT>Mostly Shady</LIGHT>
<PRICE>$2.44</PRICE>
<AVAILABILITY>031599</AVAILABILITY>
</PLANT>
<PLANT>
<COMMON>Columbine</COMMON>
<BOTANICAL>Aquilegia canadensis</BOTANICAL>
<ZONE>3</ZONE>
<LIGHT>Mostly Shady</LIGHT>
<PRICE>$9.37</PRICE>
<AVAILABILITY>030699</AVAILABILITY>
</PLANT>
|]

getPlantPrices :: Text -> [PlantPrice]
getPlantPrices = undefined

main :: IO ()
main = do
  putStrLn "hello world"
