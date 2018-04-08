{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | See the "Multinomial Naive Bayes for Text Categorization
-- Revisited" paper (present in the "data" dir of the repo). See the
-- 'animalsExample' in a source code for a usage example
module NaiveML.MultinomialNaiveBayes
  ( multinomialNaiveBayes
  , buildEnv
  , animalsExample
  ) where

import Control.Newtype
import qualified Data.Aeson as J
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

data Document cls word = Document
  { docClasses :: [cls]
  , docWords :: [word]
  } deriving (Show, Eq, Generic)

instance (J.FromJSON cls, J.FromJSON word) =>
         J.FromJSON (Document cls word)

-- | Globals used in formulas
data Env cls word = Env
  { envClasses :: [cls]
  , envDocs :: [Document cls word]
  , envVocabulary :: [word]
  } deriving (Show, Eq, Generic)

instance (J.FromJSON cls, J.FromJSON word) => J.FromJSON (Env cls word)

dlength :: [a] -> Double
dlength = fromIntegral . length

docsInClass :: Eq cls => Env cls word -> cls -> [Document cls word]
docsInClass env c = filter (\d -> c `elem` docClasses d) (envDocs env)

totalNumberOfDocs :: Env cls word -> Double
totalNumberOfDocs env = dlength (envDocs env)

-- | Pr(c)
classPrior :: Eq cls => Env cls word -> cls -> Double
classPrior env c = dlength (docsInClass env c) / totalNumberOfDocs env

-- | f_n_i
countWordInDoc :: Eq word => word -> Document cls word -> Double
countWordInDoc w d = dlength (filter (== w) (docWords d))

-- | F_x_c
wordCountInTrainingDocsClass ::
     (Eq word, Eq cls) => Env cls word -> cls -> word -> Double
wordCountInTrainingDocsClass env c w =
  sum (map (countWordInDoc w) (docsInClass env c))

vocabularySize :: Env cls word -> Double
vocabularySize env = dlength (envVocabulary env)

-- | Pr(w_n|c)
wordGivenClassProbability ::
     (Eq word, Eq cls) => Env cls word -> word -> cls -> Double
wordGivenClassProbability env w c =
  (1 + wordCountInTrainingDocsClass env c w) /
  (vocabularySize env +
   sum (map (wordCountInTrainingDocsClass env c) (envVocabulary env)))

-- | Pr(t_i|c)
documentProbability ::
     (Eq word, Eq cls) => Env cls word -> Document cls word -> cls -> Double
documentProbability env d c = product (map f (envVocabulary env))
  where
    f w = (wordGivenClassProbability env w c) ** (countWordInDoc w d)

-- | Pr(t_i)
normalizationFactor ::
     (Eq word, Eq cls) => Env cls word -> Document cls word -> Double
normalizationFactor env d = sum (map f (envClasses env))
  where
    f c = classPrior env c * documentProbability env d c

-- | Pr(c|t_i). Main working horse.
multinomialNaiveBayes :: (Eq word, Eq cls) => Env cls word -> [word] -> cls
multinomialNaiveBayes env wrds = getMax (map f (envClasses env))
  where
    f c =
      let d = (Document (error "classes not known yet") wrds)
      in ( c
         , classPrior env c * (documentProbability env d c) /
           normalizationFactor env d)
    getMax = fst . head . sortBy (flip compare `on` snd)

-- | The "training" step
buildEnv :: (Eq word, Eq cls) => [Document cls word] -> Env cls word
buildEnv docs = Env classes docs voc
  where
    classes = nub (concatMap docClasses docs)
    voc = nub (concatMap docWords docs)

-- ** Animals example from http://dataaspirant.com/2017/02/06/naive-bayes-classifier-machine-learning/
data AnimalClass
  = Parrot
  | Dog
  | Fish
  deriving (Eq, Generic, Show)

instance J.FromJSON AnimalClass

data AnimalFeatures
  = Swim
  | Wings
  | GreenColor
  | DangerousTeeth
  deriving (Eq, Generic, Show)

instance J.FromJSON AnimalFeatures

newtype AnimalDoc =
  AnimalDoc (Document AnimalClass AnimalFeatures)
  deriving (Eq, Generic, J.FromJSON)

instance Newtype AnimalDoc (Document AnimalClass AnimalFeatures) where
  pack = AnimalDoc
  unpack (AnimalDoc x) = x

loadAnimalDocs :: IO [AnimalDoc]
loadAnimalDocs = fromJust <$> J.decodeFileStrict' "data/animal_dataset.json"

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
  return ()
