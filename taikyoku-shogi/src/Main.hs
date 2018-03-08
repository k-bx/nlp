{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (foldl')
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple (swap)
import Debug.Trace
import Formatting ((%), (%.), sformat)
import Formatting.ShortFormatters (r, st)
import GHC.Generics (Generic)
import GHC.Stack
import Text.Groom (groom)

impossible :: HasCallStack => a
impossible = error "不可能！！！"

impossibleS :: (Show b, HasCallStack) => b -> a
impossibleS b = error ("不可能！！！\n" ++ groom b)

data JapaneseName = JapaneseName
  { name :: Text
  } deriving (Eq, Show, Generic)

instance IsString JapaneseName where
  fromString = JapaneseName . T.pack

data EnglishName = EnglishName
  { name :: Text
  } deriving (Eq, Show, Generic)

instance IsString EnglishName where
  fromString = EnglishName . T.pack

data NamePair = NPair
  { jpName :: Maybe JapaneseName
  , enName :: Maybe EnglishName
  } deriving (Eq, Show, Generic)

data Promotion = Promotion
  { from :: NamePair
  , to :: NamePair
  } deriving (Eq, Show, Generic)

type Table = [Promotion]

renderTable :: Table -> Text
renderTable = T.unlines . map renderPromotion

renderPromotion :: Promotion -> Text
renderPromotion (Promotion {..}) =
  sformat
    ((r 20 ' ' %. st) % " => " % (r 20 ' ' %. st))
    (renderPair from)
    (renderPair to)

renderPair :: NamePair -> Text
renderPair (NPair {..}) =
  sformat
    ((r 9 ' ' %. st) % " " % (r 18 ' ' %. st))
    (renderMJpName jpName)
    (renderMEnName enName)

renderMJpName :: Maybe JapaneseName -> Text
renderMJpName Nothing = "_____"
renderMJpName (Just (JapaneseName {..})) = name

renderMEnName :: Maybe EnglishName -> Text
renderMEnName Nothing = "_____"
renderMEnName (Just (EnglishName {..})) = name

-- | Just to make things nicer to write
instance IsString a => IsString (Maybe a) where
  fromString = Just . fromString

initialTable :: Table
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

data Rule = Rule
  { name :: Text
  , fn :: Table -> Table
  } deriving (Generic)

rules :: [Rule]
rules = [Rule "Known translations" ruleKnownTranslations]

ruleKnownTranslations :: Table -> Table
ruleKnownTranslations tbl
  -- trace ">> ruleKnownTranslations" $
  -- trace (T.unpack (renderTable tbl)) $
  -- trace ">>" $
 =
  let jpToEn :: [(Text, Text)]
      jpToEn = foldl' jpToEnF [] tbl
      jpToEnF :: [(Text, Text)] -> Promotion -> [(Text, Text)]
      jpToEnF dict Promotion {..} = dict ++ getTrans from ++ getTrans to
      getTrans :: NamePair -> [(Text, Text)]
      getTrans NPair {..} =
        case (jpName, enName) of
          (Nothing, _) -> []
          (_, Nothing) -> []
          ((Just jp), (Just en)) -> [(jp ^. field @"name", en ^. field @"name")]
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
      fillPromotion Promotion {..} = Promotion (fillNP from) (fillNP to)
      fillNP :: NamePair -> NamePair
      fillNP np@NPair {..} = fillNP' np jpName enName
      fillNP' np (Just jp) (Just en)
        | isInBothDicts jp en = np
      fillNP' _ (Just _) (Just _)
        | otherwise = impossible
      fillNP' np (Just jp) Nothing =
        let newName = fmap EnglishName (H.lookup (jp ^. field @"name") jpToEnHm)
        in np {enName = newName}
      fillNP' np Nothing (Just en) =
        let newName =
              fmap JapaneseName (H.lookup (en ^. field @"name") enToJpHm)
        in np {jpName = newName}
      fillNP' np _ _ = np
      isInBothDicts :: JapaneseName -> EnglishName -> Bool
      isInBothDicts jp en = isInJpDict jp en && isInEnDict en jp
      isInJpDict :: JapaneseName -> EnglishName -> Bool
      isInJpDict jp en =
        H.lookup (jp ^. field @"name") jpToEnHm == Just (en ^. field @"name")
      isInEnDict :: EnglishName -> JapaneseName -> Bool
      isInEnDict en jpNameJust =
        H.lookup (en ^. field @"name") enToJpHm ==
        Just (jpNameJust ^. field @"name")
      resTbl = map fillPromotion tbl
  in resTbl

type Diff = [PairChange]

data PairChange = PairChange
  { from :: Promotion
  , to :: Promotion
  , rule :: Rule
  }

solve :: Table -> (Table, [Text])
solve table =
  let (table2, diffs2) = foldl' applyRule (table, []) rules
  in case diffs2 of
       [] -> (table2, [])
       _ ->
         let (table3, diffs3) = solve table2
         in (table3, renderDiff diffs2 ++ diffs3)
    -- TODO: optimize concatenation of diffs
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

main :: IO ()
main = do
  T.putStrLn (renderTable initialTable)
  putStrLn ""
  let (result, log1) = solve initialTable
  mapM_ T.putStrLn log1
  T.putStrLn (renderTable result)
