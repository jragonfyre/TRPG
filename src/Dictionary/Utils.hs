--
-- Utils.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary.Utils where
--  ( tenseToString
--  , stringToTense
--  ) where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Data.List.Split (splitOn)
import Control.Applicative ((<$>), (<*>), (<|>))

import Dictionary.Types
import Utils (allValues)

allMoods :: [Mood]
allMoods = allValues

allSpecificities :: [Specificity]
allSpecificities = allValues

allGenders :: [Gender]
allGenders = allValues

allPersons :: [Person]
allPersons = allValues

allCases :: [Case]
allCases = allValues

allNumGens :: Countable -> [NumberGender]
allNumGens Count = [Plural] <|> (Singular <$> allGenders)
allNumGens Uncount = [NotCount]

anyNumGen :: [NumberGender]
anyNumGen = [Plural, NotCount] <|> (Singular <$> allGenders)

allSimpleTenses :: [SimpleTense]
allSimpleTenses = allValues

allTenses :: [Tense]
allTenses = Tense <$> allSimpleTenses <*> allValues <*> allValues

allVoices :: [Voice]
allVoices = allValues

tenseToString :: Tense -> String
tenseToString = do
  tense <- simpleTense
  perf <- isPerfect
  cont <- isContinuous
  return $ (map Char.toLower $ show tense) 
    ++ (if perf then "-perfect" else "")
    ++ (if cont then "-progressive" else "")

stringToTense :: String -> Maybe Tense
stringToTense str = do
  let ws = splitOn "-" str
  first <- Maybe.listToMaybe ws
  tense <- List.find ((==first) . map Char.toLower . show) allSimpleTenses
  let
    tens = Tense
      { simpleTense = tense
      , isPerfect = "perfect" `elem` ws
      , isContinuous = "progressive" `elem` ws
      }
  if (tenseToString tens) == str
  then Just tens
  else Nothing

