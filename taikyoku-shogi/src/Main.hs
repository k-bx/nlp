{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative
import Control.Lens (Lens'(..), (&), (.~), (^.))
import Data.Foldable (foldl')
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.Maybe (catMaybes, listToMaybe)

--import Control.Lens ((%~), (&), (.~), (^.), _Just)
--import Debug.Trace
import Data.Proxy (Proxy(..))
import Data.Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple (swap)
import Formatting ((%), (%.), sformat)
import Formatting.ShortFormatters (r, st)
import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits (Symbol)
import Safe
import Text.Groom (groom)

impossible :: HasCallStack => a
impossible = error "不可能！！！"

impossibleS :: (Show b, HasCallStack) => b -> a
impossibleS b = error ("不可能！！！\n" ++ groom b)

data JapaneseName = JapaneseName
  { name :: Maybe Text
  } deriving (Eq, Show, Generic)

mkJapaneseName :: Text -> JapaneseName
mkJapaneseName = JapaneseName . Just

instance IsString JapaneseName where
  fromString s = mkJapaneseName (T.pack s)

data EnglishName = EnglishName
  { name :: Maybe Text
  , weapon :: Maybe Weapon
  , movement :: Maybe Movement
  , animal :: Maybe Animal
  , military :: Maybe Military
  , material :: Maybe Material
  , position :: Maybe Position
  } deriving (Eq, Show, Generic)

mkEnglishName :: Text -> EnglishName
mkEnglishName x =
  EnglishName (Just x) Nothing Nothing Nothing Nothing Nothing Nothing

data Weapon
  = Sword
  | Bow
  | Crossbow
  deriving (Eq, Show, Generic, Read)

parseWeapon :: Text -> Maybe Weapon
parseWeapon = readMay . T.unpack

data Movement
  = Running
  | Dashing
  deriving (Eq, Show, Generic, Read)

parseMovement :: Text -> Maybe Movement
parseMovement = readMay . T.unpack

data Animal
  = Horse
  | Pig
  | Ox
  | Bear
  | Elephant
  | Wolf
  | Stag
  deriving (Eq, Show, Generic, Read)

parseAnimal :: Text -> Maybe Animal
parseAnimal = readMay . T.unpack

data Military
  = General
  | Chariot
  | Army
  | Soldier
  deriving (Eq, Show, Generic, Read)

parseMilitary :: Text -> Maybe Military
parseMilitary = readMay . T.unpack

-- TODO: White is not a material but color, think about it
data Material
  = Wood
  | Iron
  | Stone
  | White
  deriving (Eq, Show, Generic, Read)

parseMaterial :: Text -> Maybe Material
parseMaterial = readMay . T.unpack

data Position
  = PLeft
  | PRight
  deriving (Eq, Show, Generic, Read)

parsePosition :: Text -> Maybe Position
parsePosition = readMay . T.unpack . ("P" <>)

instance IsString EnglishName where
  fromString s = mkEnglishName (T.pack s)

data NamePair = NPair
  { jpName :: JapaneseName
  , enName :: EnglishName
  } deriving (Eq, Show, Generic)

data Promotion = Promotion
  { base :: NamePair
  , into :: NamePair
  } deriving (Eq, Show, Generic)

type Table = [Promotion]

renderTable :: Table -> Text
renderTable = T.unlines . map renderPromotion

renderPromotion :: Promotion -> Text
renderPromotion (Promotion {..}) =
  sformat
    ((r 40 ' ' %. st) % " => " % (r 20 ' ' %. st))
    (renderPair base)
    (renderPair into)

renderPair :: NamePair -> Text
renderPair (NPair {..}) =
  sformat
    ((r 9 ' ' %. st) % " " % (r 40 ' ' %. st))
    (renderJpName jpName)
    (renderEnName enName)

renderJpName :: JapaneseName -> Text
renderJpName JapaneseName {..} = renderName name

renderEnName :: EnglishName -> Text
renderEnName EnglishName {..} = features <> renderName name
  where
    features =
      if features' == ""
        then ""
        else features' <> " "
    features' =
      T.concat
        (List.intersperse
           " "
           (catMaybes
              [ fmap sh weapon
              , fmap sh movement
              , fmap sh animal
              , fmap sh military
              , fmap sh material
              , fmap sh position
              ]))
    sh x = "[" <> tshow x <> "]"

renderName :: IsString p => Maybe p -> p
renderName Nothing = "_____"
renderName (Just t) = t

-- | Just to make things nicer to write
instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

initialTable :: Table
initialTable =
  [ Promotion (NPair jpNone "Running Stag") (NPair "honroku" enNone)
  , Promotion (NPair jpNone enNone) (NPair "tōshō" "Sword General")
  , Promotion (NPair "gyūhei" "Ox Soldier") (NPair jpNone enNone)
  , Promotion (NPair jpNone "Right General") (NPair "ugun" enNone)
  , Promotion (NPair jpNone "Ox General") (NPair "hongyū" enNone)
  , Promotion (NPair "kyūhei" "Bow Soldier") (NPair jpNone "Bow General")
  , Promotion (NPair "kisho" "Wood General") (NPair jpNone enNone)
  , Promotion (NPair "dohei" "Crossbow Soldier") (NPair jpNone enNone)
  , Promotion (NPair jpNone enNone) (NPair jpNone "Dashing Horse")
  , Promotion (NPair jpNone "Left Chariot") (NPair jpNone enNone)
  , Promotion (NPair jpNone "Horse Soldier") (NPair "sōba" enNone)
  , Promotion (NPair "sōyū" "Running Bear") (NPair jpNone "Dashing Bear")
  , Promotion (NPair "tonshō" "Pig General") (NPair "honton" enNone)
  , Promotion (NPair "tesshō" enNone) (NPair "hakuzō" "White Elephant")
  , Promotion (NPair jpNone enNone) (NPair "sagun" "Left Army")
  , Promotion (NPair "usha" enNone) (NPair "utessha" "Right Iron Chariot")
  , Promotion (NPair jpNone enNone) (NPair "honrō" "Dashing Wolf")
  , Promotion (NPair "sekishō" "Stone General") (NPair jpNone "White Elephant")
  ]
  where
    jpNone = JapaneseName Nothing
    enNone = EnglishName Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Rule = Rule
  { name :: Text
  , fn :: Table -> Table
  } deriving (Generic)

type Diff = [PairChange]

data PairChange = PairChange
  { from :: Promotion
  , to :: Promotion
  , rule :: Rule
  }

rules :: [Rule]
rules =
  [ Rule "Known translations" ruleKnownTranslations
  , Rule "Parse features" ruleParseFeatures
  , Rule "Assume weapon stays" ruleAssumeWeaponStays
  , Rule "Assume animal stays" ruleAssumeAnimalStays
  , Rule "Assume Running -> Dashing" ruleAssumeRunningDashing
  ]

ruleKnownTranslations :: Table -> Table
ruleKnownTranslations tbl
  -- trace ">> ruleKnownTranslations" $
  -- trace (T.unpack (renderTable tbl)) $
  -- trace ">>" $
 =
  let jpToEn :: [(Text, Text)]
      jpToEn = foldl' jpToEnF [] tbl
      jpToEnF :: [(Text, Text)] -> Promotion -> [(Text, Text)]
      jpToEnF dict Promotion {..} = dict ++ getTrans base ++ getTrans into
      getTrans :: NamePair -> [(Text, Text)]
      getTrans NPair {..} =
        case (jpName ^. field @"name", enName ^. field @"name") of
          (Nothing, _) -> []
          (_, Nothing) -> []
          ((Just jp), (Just en)) -> [(jp, en)]
      jpToEnHm :: HashMap Text Text
      jpToEnHm = H.fromListWith failIfDifferent jpToEn
      failIfDifferent from to =
        if from /= to
          then impossibleS jpToEn
          else from
      enToJpHm :: HashMap Text Text
      enToJpHm =
        let swapped = map swap jpToEn
        in H.fromListWith failIfDifferent swapped
      fillPromotion :: Promotion -> Promotion
      fillPromotion Promotion {..} = Promotion (fillNP base) (fillNP into)
      fillNP :: NamePair -> NamePair
      fillNP np@NPair {..} =
        fillNP' np (jpName ^. field @"name") (enName ^. field @"name")
      fillNP' np (Just jp) (Just en)
        | isInBothDicts jp en = np
      fillNP' _ (Just _) (Just _)
        | otherwise = impossible
      fillNP' np (Just jp) Nothing =
        case H.lookup jp jpToEnHm of
          Just en -> np {enName = mkEnglishName en}
          Nothing -> np
      fillNP' np Nothing (Just en) =
        case H.lookup en enToJpHm of
          Just jp -> np {jpName = mkJapaneseName jp}
          Nothing -> np
      fillNP' np _ _ = np
      isInBothDicts :: Text -> Text -> Bool
      isInBothDicts jp en = isInJpDict jp en && isInEnDict en jp
      isInJpDict :: Text -> Text -> Bool
      isInJpDict jp en = H.lookup jp jpToEnHm == Just en
      isInEnDict :: Text -> Text -> Bool
      isInEnDict en jp = H.lookup (en) enToJpHm == Just jp
      resTbl = map fillPromotion tbl
  in resTbl

-- TODO: replace listToMaybe with impossible upon multiple different candidates
ruleParseFeatures :: Table -> Table
ruleParseFeatures = map parsePromotion
  where
    parsePromotion Promotion {..} = Promotion (parseNP base) (parseNP into)
    parseNP NPair {..} = NPair (parseJP jpName) (parseEN enName)
    parseJP = id
    parseEN en@EnglishName {name = Nothing, ..} = en
    parseEN en@EnglishName {name = Just n, ..} =
      let wrds = T.words n
      in en
         { weapon = weapon <|> p wrds parseWeapon
         , movement = movement <|> p wrds parseMovement
         , animal = animal <|> p wrds parseAnimal
         , military = military <|> p wrds parseMilitary
         , material = material <|> p wrds parseMaterial
         , position = position <|> p wrds parsePosition
         }
    p wrds f = listToMaybe (catMaybes (map f wrds))

-- | Based on 'Bow Soldier' -> 'Bow General'
ruleAssumeWeaponStays :: Table -> Table
ruleAssumeWeaponStays = map goPromotion
  where
    goPromotion = goFrom . goTo
    goFrom p =
      case (p ^. field @"base" . field @"enName" . field @"weapon") of
        Just wpn ->
          p & field @"into" . field @"enName" . field @"weapon" .~ Just wpn
        Nothing -> p
    goTo p =
      case (p ^. field @"into" . field @"enName" . field @"weapon") of
        Just wpn ->
          p & field @"base" . field @"enName" . field @"weapon" .~ Just wpn
        Nothing -> p

-- | This rule cannot go from right to left becase while we see that
-- Bear stays (Running Bear -> Dashing Bear), we also see that it's
-- not working backwards (Stone General -> White Elephant)
ruleAssumeAnimalStays :: Table -> Table
ruleAssumeAnimalStays = map goPromotion
  where
    goPromotion = goFrom
    goFrom p =
      case (p ^. field @"base" . field @"enName" . field @"animal") of
        Just wpn ->
          p & field @"into" . field @"enName" . field @"animal" .~ Just wpn
        Nothing -> p

-- | 'Running Bear' -> 'Dashing Bear'
ruleAssumeRunningDashing :: Table -> Table
ruleAssumeRunningDashing = map goPromotion
  where
    goPromotion = goFrom . goTo
    goFrom p =
      case (p ^. field @"base" . field @"enName" . field @"movement") of
        Just Running ->
          p & field @"into" . field @"enName" . field @"movement" .~ Just Dashing
        _ -> p
    goTo p =
      case (p ^. field @"into" . field @"enName" . field @"movement") of
        Just Dashing ->
          p & field @"base" . field @"enName" . field @"movement" .~ Just Running
        _ -> p

-- promotionTwoSidedFeatureStaysRule ::
--      Lens' EnglishName a -> Promotion -> Promotion
-- promotionTwoSidedFeatureStaysRule l = goFromTo . goToFrom
--   where
--     goFromTo p@Promotion {..} =
--       case from ^. field @"enName" . l of
--         Just wpn -> p & field @"to" . field @"enName" . l .~ Just wpn
--         Nothing -> p
--     goToFrom p@Promotion {..} =
--       case to ^. field @"enName" . l of
--         Just wpn ->
--           p & field @"from" . field @"enName" . l .~ Just wpn
--         Nothing -> p
-- TODO: optimize concatenation of diffs
solve :: Table -> (Table, [Text])
solve table =
  let (table2, diffs2) = foldl' applyRule (table, []) rules
  in case diffs2 of
       [] -> (table2, [])
       _ ->
         let (table3, diffs3) = solve table2
         in (table3, renderDiff diffs2 ++ diffs3)
  where
    applyRule :: (Table, Diff) -> Rule -> (Table, Diff)
    applyRule (tbl, diff1) (rule@Rule {..}) =
      let tbl2 = fn tbl
          diff2 = diffTable rule tbl tbl2
      in (tbl2, diff1 ++ diff2)
    diffTable :: Rule -> Table -> Table -> Diff
    diffTable (rule@Rule {..}) tbl1 tbl2 =
      let zipped = zip tbl1 tbl2
          compared = map (uncurry (==)) zipped
          indexed = zip [0 ..] compared
          filtered = filter ((== False) . snd) indexed
          indexes = map fst filtered
          pairs = map (\i -> (tbl1 !! i, tbl2 !! i)) indexes
          toPairChange (p1, p2) = PairChange {from = p1, to = p2, rule = rule}
          diff1 = map toPairChange pairs
      in diff1
    renderDiff :: Diff -> [Text]
    renderDiff = concatMap renderChange
    renderChange (PairChange {..}) =
      [ sformat (" **RULE** " % st % ":") (rule ^. field @"name")
      , "  " <> renderPromotion from
      , "  " <> renderPromotion to
      , ""
      ]

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = do
  T.putStrLn (renderTable initialTable)
  putStrLn ""
  let (result, log1) = solve initialTable
  mapM_ T.putStrLn log1
  T.putStrLn (renderTable result)
