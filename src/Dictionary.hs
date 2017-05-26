--
-- Dictionary.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary where
--  (
--  ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Char (isLetter)
import Control.Applicative ((<$>), (<*>), (<|>))
import Utils (allValues)


-- note that this is all for English. Translating this would be a bitch.
-- We'll see if we can make this more generic later...

-- properties of nouns dictionary forms
data Gender = Male | Female | Neuter | Other
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allGenders :: [Gender]
allGenders = allValues

{-
instance FromJSON Gender where
  parseJSON (String "male") = Male
  parseJSON (String "female") = Female
  parseJSON (String "neuter") = Neuter
  parseJSON (String "other") = Other

instance ToJSON Gender where
  toJSON Male = 
-}

data Person = First | Second | Third
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allPersons :: [Person]
allPersons = allValues

data Countable = Count | Uncount
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- properties of inflected forms
--data Number = Singular | Plural | NotCount
data Case = Subjective | Objective
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allCases :: [Case]
allCases = allValues

data NumberGender = Singular Gender | Plural | NotCount
  deriving (Show, Read, Eq, Ord)

allNumGens :: Countable -> [NumberGender]
allNumGens Count = [Plural] <|> (Singular <$> allGenders)
allNumGens Uncount = [NotCount]

anyNumGen :: [NumberGender]
anyNumGen = [Plural, NotCount] <|> (Singular <$> allGenders)

data NounBaseType
  -- nouns might have a variety of allowed numbergenders
  = SimpleNounBase Countable [NumberGender]
  deriving (Show, Read, Eq, Ord)

data NounType
  = SimpleNoun NumberGender -- Case
  deriving (Show, Read, Eq, Ord)
  
--  | AdjectiveNess

{-
class Nominal a where
  nomGender :: a -> [Gender]
  nomNumber :: a -> [Number]
  nomCase :: a -> [Case]
  specifiity :: a -> Maybe Specificity
-}



-- properties of Adjectives
-- types of basic words that are adjectives
data AdjectiveBaseType
  = DeterminerBase Specificity [NumberGender] -- numbers to which the determiner is applicable
  | SimpleAdjectiveBase
  deriving (Show, Read, Eq, Ord)

-- types of words that functions as adjectives
data AdjectiveType
  = Determiner NumberGender
  | SimpleAdjective
  | PresentParticiple
  | PastParticiple
--  | Comparative
--  | Superlative
  | Possessive
  deriving (Show, Read, Eq, Ord)

data Specificity = Specific | General
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allSpecificities :: [Specificity]
allSpecificities = allValues

-- properties of Verbs
data VerbBaseType = SimpleVerbBase Transitivity
  deriving (Show, Read, Eq, Ord)

data Transitivity = Intransitive | Transitive | Ditransitive
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Mood 
  = Indicative
  | Imperative -- just drop the you in an otherwise valid indicative second person sentence
--  | Interrogative -- not relevant rn
--  | Conditional -- also not relevant rn
--  | Subjunctive -- also not relevant rn
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allMoods :: [Mood]
allMoods = allValues

-- no need for future tense rn, cuts down on the size of things for now.
data SimpleTense = Past | Present -- | Future
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allSimpleTenses :: [SimpleTense]
allSimpleTenses = allValues

data Tense = Tense
  { simpleTense :: SimpleTense 
  , isPerfect :: Bool -- auxiliary verb have + pp
  , isContinuous :: Bool -- auxiliary verb + ger
  }
  deriving (Show, Read, Eq, Ord)

allTenses :: [Tense]
allTenses = Tense <$> allSimpleTenses <*> allValues <*> allValues

-- no need for Passive voice rn
data Voice = ActiveVoice -- | PassiveVoice -- auxiliary verb be + pp
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

allVoices :: [Voice]
allVoices = allValues

-- properties of inflected forms that are verbs
-- all verb types should be able to be conjugated for Mood, Tense, Person, Number
data VerbType
  = SimpleVerb Mood Tense Voice Person NumberGender
  deriving (Show, Read, Eq, Ord)




data PronounBaseType
  = PronounBase Person Specificity [NumberGender]
  deriving (Show, Read, Eq, Ord)

-- basically we have two dictionaries,
-- one which stores the dictionary form of a word
-- and all data needed to construct its inflections
-- and their data
--
-- the second dictionary is from inflections to properties

-- different kinds of parts of speech have different inflection patterns even within the same part of speech,
-- this determines both the part of speech and inflection pattern (recorded as a field)
-- really this is dictionary info
data POSType
  = NounT NounBaseType
  -- given a noun, e.g. sword, we have that it is count neuter
  -- note all nouns are third person, yes they are. You wouldn't say Danny are if you're talking to Danny
  | PronounT PronounBaseType
  -- in English all pronouns are 
  | AdjectiveT AdjectiveBaseType
  -- adjectives don't inflect, but they do have properties like quantifier, determiner etc.
  | VerbT Transitivity -- currently no data
  | AdverbT
  | PrepositionT -- no inflection, and temporarily no properties
  deriving (Show, Read, Eq, Ord)

--data NounType = Nou


-- part of speech of a single word
-- don't duplicate the information in the base type, since inflected entries are associated 
-- to their base entries
-- really this describes how the word has been inflected
data POS
  = Noun NounType
  | Pronoun NumberGender Case
  | Adjective AdjectiveType
  | Verb VerbType
  | Adverb
  | Preposition -- again, no inflection
  deriving (Show, Read, Eq, Ord)

possibleInflectedPOSs :: POSType -> [POS]
possibleInflectedPOSs (NounT (SimpleNounBase Count numgen)) = [Adjective Possessive] <|> Noun . SimpleNoun
  <$> numgen
  -- <*> [Subjective, Objective]
possibleInflectedPOSs (NounT (SimpleNounBase Uncount _)) = return . Noun $ SimpleNoun NotCount
  -- <$> [Subjective, Objective]
possibleInflectedPOSs (PronounT (PronounBase _ _ numgen)) = Pronoun
  <$> numgen
  <*> allCases
possibleInflectedPOSs (AdjectiveT (DeterminerBase _ numgen)) = Adjective . Determiner <$> numgen
possibleInflectedPOSs (AdjectiveT (SimpleAdjectiveBase)) = return $ Adjective SimpleAdjective
possibleInflectedPOSs (VerbT _) = [Adjective PastParticiple, Adjective PresentParticiple]
  <|> (fmap Verb $ SimpleVerb
    <$> allMoods
    <*> allTenses
    <*> allVoices
    <*> allPersons
    <*> anyNumGen)
possibleInflectedPOSs AdverbT = return $ Adverb
possibleInflectedPOSs PrepositionT = return $ Preposition

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

regularPlural :: String -> String
regularPlural noun = reverse . regPluralHelper $ reverse noun
  where
    -- common irregulars, which we will incorporate into the rule
    -- because they are more common than not it seems
    regPluralHelper ('e':'f':nb) = "sev"++nb
    regPluralHelper ('f':nb) = "sev"++nb 
    regPluralHelper nb@('o':_) = "se"++nb
    regPluralHelper ('s':'u':nb) = 'i':nb
    regPluralHelper ('s':'i':nb) = "se" ++ nb
    regPluralHelper ('n':'o':nb) = 'a':nb
    -- regular rules
    regPluralHelper nb@('x':_) = "se"++nb
    regPluralHelper nb@('s':_) = "se"++nb
    regPluralHelper nb@('h':'c':_) = "se"++nb
    regPluralHelper nb@('h':'s':_) = "se"++nb
    regPluralHelper nb1@('y':l:nb) 
      = if isConsonant l
        then "sei"++(l:nb)
        else 's':nb1
    -- default rule
    regPluralHelper nb = 's':nb

regularPast :: String -> String
regularPast = regularPastParticiple 

regularPastParticiple :: String -> String
regularPastParticiple verb = reverse . regPPHelper $ reverse verb
  where
    regPPHelper vb@('e':_) = 'd':vb
    regPPHelper vb = "de"++vb

regularPresentParticiple :: String -> String
regularPresentParticiple verb = reverse . regPrPHelper $ reverse verb
  where
    regPrPHelper ('e':vb) = "gni" ++ vb
    regPrPHelper vb = "gni" ++ vb

-- takes a dictionary form word, its POS info, the desired inflection's form, and produces the inflected form.
--inflect :: String -> POSType -> POS -> String


data BaseEntry = BaseEntry
  { baseText :: String
  , partOfSpeechType :: POSType
  , irregularForms :: Map POS String
  }
  deriving (Show, Read, Eq, Ord)

--generateEntries :: BaseEntry -> [Entry]

-- 
data Entry = Entry
  { entryText :: String
  , partOfSpeech :: POS
  , baseForm :: BaseEntry
  }
  deriving (Show, Read, Eq, Ord)



