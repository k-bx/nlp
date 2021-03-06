{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | See the "Multinomial Naive Bayes for Text Categorization
-- Revisited" paper (present in the "data" dir of the repo). See the
-- 'animalsExample' in a source code for a usage example
module NaiveML.MultinomialNaiveBayes
  ( multinomialNaiveBayes
  , transformedWeightNormalizedComplementNaiveBayes
  , buildEnv
  , animalsExample
  , tfidf
  , Document(..)
  , Env
  ) where

import Control.Newtype
import qualified Data.Aeson as J
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.List (nub, sortBy)
import Data.Maybe (fromMaybe)
import Data.Maybe (fromJust)
import qualified Data.String.Class as S
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

data Document cls word = Document
  { docClasses :: [cls]
  , docWords :: [word]
  } deriving (Show, Eq, Generic)

instance (J.FromJSON cls, J.FromJSON word) =>
         J.FromJSON (Document cls word)

data EfficiencyCache cls word = EfficiencyCache
  { docsPerClass :: HashMap cls [Document cls word]
  , docsWithWordNum :: HashMap word Int
  , docsTotalNumber :: Double
  , effVocabularySize :: Double
  } deriving (Show, Eq, Generic)

-- | Globals used in formulas
data Env cls word = Env
  { envClasses :: [cls]
  , envDocs :: [Document cls word]
  , envVocabulary :: [word]
  , envEfficiencyCache :: EfficiencyCache cls word -- ^ added purely for lookup efficiency
  } deriving (Show, Eq, Generic)

dlength :: [a] -> Double
dlength = fromIntegral . length

docsInClass ::
     (Eq cls, Hashable cls) => Env cls word -> cls -> [Document cls word]
docsInClass env c =
  fromMaybe [] (H.lookup c (docsPerClass (envEfficiencyCache env)))
  where
    _noCacheImp = filter (\d -> c `elem` docClasses d) (envDocs env)

countDocsWithWord :: (Eq word, Hashable word) => Env cls word -> word -> Double
countDocsWithWord env w =
  fromIntegral $
  fromMaybe 0 (H.lookup w (docsWithWordNum (envEfficiencyCache env)))
  where
    _noCacheImp = dlength (filter (\d -> w `elem` docWords d) (envDocs env))

totalNumberOfDocs :: Env cls word -> Double
totalNumberOfDocs env = docsTotalNumber (envEfficiencyCache env)
  where
    _noCacheImp = dlength (envDocs env)

-- | Pr(c)
classPrior :: (Eq cls, Hashable cls) => Env cls word -> cls -> Double
classPrior env c = dlength (docsInClass env c) / totalNumberOfDocs env

-- | f_n_i
countWordInDoc :: Eq word => word -> Document cls word -> Double
countWordInDoc w d = dlength (filter (== w) (docWords d))

-- | F_x_c
wordCountInTrainingDocsClass ::
     (Eq word, Eq cls, Hashable cls) => Env cls word -> cls -> word -> Double
wordCountInTrainingDocsClass env c w =
  sum (map (countWordInDoc w) (docsInClass env c))

-- | N
vocabularySize :: Env cls word -> Double
vocabularySize env = effVocabularySize (envEfficiencyCache env)
  where
    _noCacheImp = dlength (envVocabulary env)

-- | Pr(w_n|c)
wordGivenClassProbability ::
     (Eq word, Eq cls, Hashable cls) => Env cls word -> word -> cls -> Double
wordGivenClassProbability env w c =
  (1 + wordCountInTrainingDocsClass env c w) /
  (vocabularySize env +
   sum (map (wordCountInTrainingDocsClass env c) (envVocabulary env)))

-- | Pr(t_i|c)
documentProbability ::
     (Eq word, Eq cls, Hashable cls)
  => Env cls word
  -> Document cls word
  -> cls
  -> Double
documentProbability env d c = product (map f (envVocabulary env))
  where
    f w = (wordGivenClassProbability env w c) ** (countWordInDoc w d)

-- | Pr(t_i)
normalizationFactor ::
     (Eq word, Eq cls, Hashable cls)
  => Env cls word
  -> Document cls word
  -> Double
normalizationFactor env d = sum (map f (envClasses env))
  where
    f c = classPrior env c * documentProbability env d c

-- | Pr(c|t_i). Main working horse.
multinomialNaiveBayes ::
     (Eq word, Eq cls, Hashable cls) => Env cls word -> [word] -> cls
multinomialNaiveBayes env wrds = getMax (map f (envClasses env))
  where
    d = Document (error "classes not known yet") wrds
    f c =
      ( c
      , classPrior env c * (documentProbability env d c) /
        normalizationFactor env d)
    getMax = fst . head . sortBy (flip compare `on` snd)

-- | transforms original word's frequency
tfidf ::
     (Eq word, Hashable word, Eq cls)
  => Env cls word
  -> Document cls word
  -> word
  -> Double
tfidf env doc word =
  log (countWordInDoc word doc + 1) *
  log (totalNumberOfDocs env / countDocsWithWord env word)

-- | All classes but the one in the argument
complementClasses :: Eq cls => Env cls word -> cls -> [cls]
complementClasses env c = filter (/= c) (envClasses env)

-- | w_n_c
wordWeight ::
     (Eq word, Eq cls, Hashable cls) => Env cls word -> word -> cls -> Double
wordWeight env word cls = log ((1 + s1) / vocabularySize env + s2)
  where
    s1 = sum (map s1f (complementClasses env cls))
    s1f c = wordCountInTrainingDocsClass env c word
    s2 = sum (map s2f (complementClasses env cls))
    s2f c = sum (map (s2f2 c) (envVocabulary env))
    s2f2 c w = wordCountInTrainingDocsClass env c w

-- | TWCNB
transformedWeightNormalizedComplementNaiveBayes ::
     (Eq word, Eq cls, Hashable cls) => Env cls word -> [word] -> cls
transformedWeightNormalizedComplementNaiveBayes env wrds =
  getMin (map f (envClasses env))
  where
    d = Document (error "classes not known yet") wrds
    f c = (c, sum (map (f2 c) wrds))
    f2 c w = countWordInDoc w d * wordWeight env w c
    getMin = fst . head . sortBy (compare `on` snd)

-- | The "training" step
buildEnv ::
     (Eq word, Hashable word, Eq cls, Hashable cls)
  => [Document cls word]
  -> Env cls word
buildEnv docs =
  Env
    classes
    docs
    voc
    (EfficiencyCache
       docsPerClass'
       docsWithWordNum'
       docsTotalNumber'
       effVocabularySize')
  where
    classes = nub (concatMap docClasses docs)
    voc = nub (concatMap docWords docs)
    docsPerClass' =
      H.fromListWith
        (++)
        (concatMap
           (\doc -> concatMap (\c -> [(c, [doc])]) (docClasses doc))
           docs)
    docsWithWordNum' =
      H.fromListWith
        (+)
        (concatMap (\doc -> concatMap (\w -> [(w, 1)]) (docWords doc)) docs)
    docsTotalNumber' = dlength docs
    effVocabularySize' = dlength voc

-- ** Animals example from http://dataaspirant.com/2017/02/06/naive-bayes-classifier-machine-learning/
data AnimalClass
  = Parrot
  | Dog
  | Fish
  deriving (Eq, Generic, Show, Hashable)

instance J.FromJSON AnimalClass

data AnimalFeatures
  = Swim
  | Wings
  | GreenColor
  | DangerousTeeth
  deriving (Eq, Generic, Show, Hashable)

instance J.FromJSON AnimalFeatures

newtype AnimalDoc =
  AnimalDoc (Document AnimalClass AnimalFeatures)
  deriving (Eq, Generic, J.FromJSON)

instance Newtype AnimalDoc (Document AnimalClass AnimalFeatures) where
  pack = AnimalDoc
  unpack (AnimalDoc x) = x

loadAnimalDocs :: IO [AnimalDoc]
loadAnimalDocs = do
  t <- T.readFile "data/animal_dataset.json"
  return . fromJust . J.decode . S.fromText $ t

-- | Run this from your repl to see the example usage. See the source.
animalsExample :: IO ()
animalsExample = do
  animals <- loadAnimalDocs
  let env = buildEnv (map unpack animals)
  let predictFor1 = [Swim, GreenColor]
  putStrLn $ "Predicting for these features: " ++ show predictFor1
  print $ predictFor1
  putStrLn $ "Resulting class is:"
  print $ multinomialNaiveBayes env predictFor1
  let predictFor2 = [Swim, DangerousTeeth]
  putStrLn $ "Predicting for these features: " ++ show predictFor2
  print $ predictFor2
  putStrLn $ "Resulting class is:"
  print $ multinomialNaiveBayes env predictFor2
  putStrLn $ "TWCNB Predicting for these features: " ++ show predictFor1
  print $ predictFor1
  putStrLn $ "TWCNB Resulting class is:"
  print $ transformedWeightNormalizedComplementNaiveBayes env predictFor1
  putStrLn $ "TWCNB Predicting for these features: " ++ show predictFor2
  print $ predictFor2
  putStrLn $ "TWCNB Resulting class is:"
  print $ transformedWeightNormalizedComplementNaiveBayes env predictFor2
  return ()
