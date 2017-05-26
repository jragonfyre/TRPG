--
-- Dictionary.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary where
--  (
--  ) where


-- properties of nouns dictionary forms
data Gender = Male | Female | Neuter | Other
data Person = First | Second | Third
data NounType = Count | Uncount

-- properties of inflected forms
data Number = Singular | Plural | Uncount
data Case = Subjective | Objective

-- basically we have two dictionaries,
-- one which stores the dictionary form of a word
-- and all data needed to construct its inflections
-- and their data
--
-- the second dictionary is from inflections to properties

-- different kinds of parts of speech have different inflection patterns even within the same part of speech,
-- this determines both the part of speech and inflection pattern (recorded as a field)
data POSType
  = NounT NounType Gender Person 
  | AdjectiveT
  | VerbT
  | AdverbT

data POS
  = Noun Number Gender Person Case
  | Adjective
  | Verb
  | Adverb

possibleInflectedPOSs :: POSType -> [POS]
possibleInflectedPOSs (NounT Count g p) = Noun
  <$> [Singular, Plural]
  <*> return g
  <*> return p
  <*> [Subjective, Objective]
possibleInflectedPOSs (NounT Uncount g p) = Noun Uncount g p
  <$> [Subjective, Objective]
possibleInflectedPOSs AdjectiveT = Adjective
possibleInflectedPOSs VerbT = Verb
possibleInflectedPOSs AdverbT = Adverb



-- 
data Entry =
  { PartOfSpeech
  }



