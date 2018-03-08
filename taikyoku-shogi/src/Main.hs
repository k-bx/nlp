{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Records

data JapaneseName = JapaneseName
  { name :: Text
  } deriving (Eq, Show)

instance IsString JapaneseName where
  fromString = JapaneseName . T.pack

data EnglishName = EnglishName
  { name :: Text
  } deriving (Eq, Show)

instance IsString EnglishName where
  fromString = EnglishName . T.pack

data NamePair = NPair
  { jpName :: Maybe JapaneseName
  , enName :: Maybe EnglishName
  } deriving (Eq, Show)

data Promotion = Promotion
  { from :: NamePair
  , to :: NamePair
  } deriving (Eq, Show)

type PromotionTable = [Promotion]

renderPromotionTable :: PromotionTable -> Text
renderPromotionTable = T.unlines . map renderLine
  where
    renderLine (Promotion {..}) = renderPair from <> "\t =>\t " <> renderPair to
    renderPair (NPair {..}) =
      renderMJpName jpName <> "\t" <> renderMEnName enName
    renderMJpName Nothing = "_____"
    renderMJpName (Just (JapaneseName {..})) = name
    renderMEnName Nothing = "_____"
    renderMEnName (Just (EnglishName {..})) = name

-- | Just to make things nicer to write
instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

initialTable :: PromotionTable
initialTable =
  [ Promotion (NPair Nothing "Running Stag") (NPair "honroku" Nothing)
  , Promotion (NPair Nothing Nothing) (NPair "tōshō" "Sword General")
  , Promotion (NPair "gyūhei" "Ox Soldier") (NPair Nothing Nothing)
  , Promotion (NPair Nothing "Right General") (NPair "ugun" Nothing)
  , Promotion (NPair Nothing "Ox General") (NPair "hongyū" Nothing)
  , Promotion (NPair "kyūhei" "Bow Soldier") (NPair Nothing "Bow General")
  , Promotion (NPair "kisho" "Wood General") (NPair Nothing Nothing)
  , Promotion (NPair "dohei" "Crossbow Soldier") (NPair Nothing Nothing)
  , Promotion (NPair Nothing Nothing) (NPair Nothing "Dashing Horse")
  , Promotion (NPair Nothing "Left Chariot") (NPair Nothing Nothing)
  , Promotion (NPair Nothing "Horse Soldier") (NPair "sōba" Nothing)
  , Promotion (NPair "sōyū" "Running Bear") (NPair Nothing "Dashing Bear")
  , Promotion (NPair "tonshō" "Pig General") (NPair "honton" Nothing)
  , Promotion (NPair "tesshō" Nothing) (NPair "hakuzō" "White Elephant")
  , Promotion (NPair Nothing Nothing) (NPair "sagun" "Left Army")
  , Promotion (NPair "usha" Nothing) (NPair "utessha" "Right Iron Chariot")
  , Promotion (NPair Nothing Nothing) (NPair "honrō" "Dashing Wolf")
  , Promotion (NPair "sekishō" "Stone General") (NPair Nothing "White Elephant")
  ]

main :: IO ()
main = do
  putStrLn "hello world"
