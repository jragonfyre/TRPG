--
-- Types.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary.Types
--  (
--  ) where

-- properties of nouns dictionary forms
data Gender = Male | Female | Neuter | Other
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Person = First | Second | Third
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Countable = Count | Uncount
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Case = Subjective | Objective
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data NumberGender = Singular Gender | Plural | NotCount
  deriving (Show, Read, Eq, Ord, Generic)

data NounBaseType
  -- nouns might have a variety of allowed numbergenders
  = SimpleNounBase Countable [NumberGender]
  deriving (Show, Read, Eq, Ord, Generic)

data NounType
  = SimpleNoun NumberGender -- Case
--  | AdjectiveNess
  deriving (Show, Read, Eq, Ord, Generic)

-- properties of Adjectives
-- types of basic words that are adjectives
data AdjectiveBaseType
  = DeterminerBase Specificity [NumberGender] -- numbers to which the determiner is applicable
  | SimpleAdjectiveBase
  deriving (Show, Read, Eq, Ord, Generic)

-- types of words that functions as adjectives
data AdjectiveType
  = Determiner NumberGender
  | SimpleAdjective
  | PresentParticiple
  | PastParticiple
--  | Comparative
--  | Superlative
  | Possessive
  deriving (Show, Read, Eq, Ord, Generic)

data Specificity = Specific | General
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- properties of Verbs
data VerbBaseType 
  = SimpleVerbBase Transitivity
  | HelperVerb
  | Copula
  deriving (Show, Read, Eq, Ord, Generic)

data Transitivity = Intransitive | Transitive | Ditransitive
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Mood 
  = Indicative
  | Imperative -- just drop the you in an otherwise valid indicative second person sentence
--  | Potential
--  | Interrogative -- not relevant rn
--  | Conditional -- also not relevant rn
--  | Subjunctive -- also not relevant rn
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- no need for future tense rn, cuts down on the size of things for now.
data SimpleTense = Past | Present -- | Future
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Tense = Tense
  { simpleTense :: SimpleTense 
  , isPerfect :: Bool -- auxiliary verb have + pp
  , isContinuous :: Bool -- auxiliary verb + ger
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- no need for Passive voice rn
data Voice = ActiveVoice -- | PassiveVoice -- auxiliary verb be + pp
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- properties of inflected forms that are verbs
-- all verb types should be able to be conjugated for Mood, Tense, Person, Number
data VerbType
  = SimpleVerb 
      { verbMood :: Mood
      , verbTense :: Tense
      , verbVoice :: Voice
      , verbPerson :: Person
      , verbNumber :: NumberGender 
      }
  -- this is a problem, the valid tenses depend on the mood...
  deriving (Show, Read, Eq, Ord, Generic)

data PronounBaseType
  = PronounBase Person Specificity [NumberGender]
  deriving (Show, Read, Eq, Ord, Generic)

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
  | VerbT VerbBaseType -- currently no data
  | AdverbT
  | PrepositionT -- no inflection, and temporarily no properties
  deriving (Show, Read, Eq, Ord, Generic)

data SimpleInflection
  = InflPlural -- for nouns
  | InflThirdPersonPresent
  | InflSimplePast
  | InflPastParticiple
  | InflPresentParticiple
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

data Irregularities 
  = IrregularCopula
  -- | IrregularHave
  | IrregularSimple (Map SimpleInflection String)
  deriving (Show, Read, Eq, Ord, Generic)

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
  deriving (Show, Read, Eq, Ord, Generic)

data BaseEntry = BaseEntry
  { baseText :: String
  , partOfSpeechType :: POSType
  , irregularities :: Maybe Irregularities
  }
  deriving (Show, Read, Eq, Ord, Generic)

data InflectedEntry = InflectedEntry
  { entryText :: String
  , partOfSpeech :: POS
  , baseForm :: BaseEntry
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- takes a base entry, wordText, position
data WordEntry = WordEntry
  { wordText :: String
  , baseInflection :: InflectedEntry 
  , wordPosition :: Maybe (Int, Int)
  }
  deriving (Show, Read, Eq, Ord, Generic)



