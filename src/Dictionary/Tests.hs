--
-- DictionaryTests.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module DictionaryTests where
--  (
--  ) where

import Dictionary

{-
data POS
  = Noun NounType
  | Pronoun NumberGender Case
  | Adjective AdjectiveType
  | Verb VerbType
  | Adverb
  | Preposition -- again, no inflection
  deriving (Show, Read, Eq, Ord, Generic)
-}

isNoun :: InflectedEntry -> Bool
isNoun InflectedEntry{partOfSpeech=(Noun _)} = True
isNoun _ = False

isVerb :: InflectedEntry -> Bool
isVerb InflectedEntry{partOfSpeech=(Verb _)} = True
isVerb _ = False

isAdjective :: InflectedEntry -> Bool
isAdjective InflectedEntry{partOfSpeech=(Adjective _)} = True
isAdjective _ = False

isAdverb :: InflectedEntry -> Bool
isAdverb InflectedEntry{partOfSpeech=(Adverb)} = True
isAdverb _ = False


isPronoun :: InflectedEntry -> Bool
isPronoun InflectedEntry{partOfSpeech=(Adjective _)} = True
isPronoun _ = False

isPreposition :: InflectedEntry -> Bool
isPreposition InflectedEntry{partOfSpeech=(Adverb)} = True
isPreposition _ = False

