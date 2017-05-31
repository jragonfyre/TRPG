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

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)

import Data.Char (isLetter)
import Control.Applicative ((<$>), (<*>), (<|>))
import Utils (allValues)
import Data.Maybe (mapMaybe, fromMaybe)

import GHC.Generics
import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey (..), FromJSONKey (..))
import qualified Data.Aeson as Aes
import qualified Data.Aeson.Types as Aes
import ParseUtilities

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), ToJSON (..))

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text

import Data.List.Split (splitOn)

import Data.Hashable (Hashable (..))

import Data.Monoid ((<>), Monoid (..))


-- note that this is all for English. Translating this would be a bitch.
-- We'll see if we can make this more generic later...

-- properties of nouns dictionary forms
data Gender = Male | Female | Neuter | Other
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Gender where
  parseJSON = genericParseJSON
instance ToJSON Gender where
  toJSON = genericToJSON

allGenders :: [Gender]
allGenders = allValues

data Person = First | Second | Third
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Person where
  parseJSON = genericParseJSON
instance ToJSON Person where
  toJSON = genericToJSON

allPersons :: [Person]
allPersons = allValues

data Countable = Count | Uncount
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Countable where
  parseJSON = genericParseJSON
instance ToJSON Countable where
  toJSON = genericToJSON

-- properties of inflected forms
--data Number = Singular | Plural | NotCount
data Case = Subjective | Objective
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Case where
  parseJSON = genericParseJSON
instance ToJSON Case where
  toJSON = genericToJSON

allCases :: [Case]
allCases = allValues

data NumberGender = Singular Gender | Plural | NotCount
  deriving (Show, Read, Eq, Ord, Generic)

numberGenderOptions :: MyOptions NumberGender
numberGenderOptions  
  = (defOptions :: MyOptions NumberGender)
      { nullaries = [Plural, NotCount]
      , constructorDictionary = HM.fromList
          [ ("NotCount", "Uncount")
          ]
      }

instance FromJSON NumberGender where
  --parseJSON (Y.String "plural") = return Plural
  --parseJSON (Y.String "uncount") = return NotCount
  parseJSON x = genParseJSONWithOpts numberGenderOptions x
instance ToJSON NumberGender where
  --toJSON Plural = Y.String "plural"
  --toJSON NotCount = Y.String "uncount"
  toJSON x = genToJSONWithOpts numberGenderOptions x

allNumGens :: Countable -> [NumberGender]
allNumGens Count = [Plural] <|> (Singular <$> allGenders)
allNumGens Uncount = [NotCount]

anyNumGen :: [NumberGender]
anyNumGen = [Plural, NotCount] <|> (Singular <$> allGenders)

data NounBaseType
  -- nouns might have a variety of allowed numbergenders
  = SimpleNounBase Countable [NumberGender]
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON NounBaseType where
  parseJSON = genericParseJSON
instance ToJSON NounBaseType where
  --toJSON (SimpleNounBase count []) = 
  toJSON = genericToJSON

data NounType
  = SimpleNoun NumberGender -- Case
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON NounType where
  parseJSON = genericParseJSON
instance ToJSON NounType where
  toJSON = genericToJSON
  
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
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON AdjectiveBaseType where
  parseJSON = genericParseJSON
instance ToJSON AdjectiveBaseType where
  toJSON = genericToJSON

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

adjTypeOptions :: MyOptions AdjectiveType
adjTypeOptions  
  = (defOptions :: MyOptions AdjectiveType)
      { nullaries =
          [ SimpleAdjective
          , PresentParticiple
          , PastParticiple
          , Possessive
          ]
      }

instance FromJSON AdjectiveType where
  parseJSON = genParseJSONWithOpts adjTypeOptions
instance ToJSON AdjectiveType where
  toJSON = genToJSONWithOpts adjTypeOptions

data Specificity = Specific | General
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Specificity where
  parseJSON = genericParseJSON
instance ToJSON Specificity where
  toJSON = genericToJSON

allSpecificities :: [Specificity]
allSpecificities = allValues

-- properties of Verbs
data VerbBaseType 
  = SimpleVerbBase Transitivity
  | HelperVerb
  | Copula
  deriving (Show, Read, Eq, Ord, Generic)

verbBaseTypeOptions :: MyOptions VerbBaseType
verbBaseTypeOptions  
  = (defOptions :: MyOptions VerbBaseType)
      { nullaries =
          [ HelperVerb
          , Copula
          ]
      , constructorDictionary = HM.fromList
          [ ("SimpleVerbBase", "SimpleVerb")
          ]
      }

instance FromJSON VerbBaseType where
  parseJSON = genParseJSONWithOpts verbBaseTypeOptions
instance ToJSON VerbBaseType where
  toJSON = genToJSONWithOpts verbBaseTypeOptions

data Transitivity = Intransitive | Transitive | Ditransitive
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Transitivity where
  parseJSON = genericParseJSON
instance ToJSON Transitivity where
  toJSON = genericToJSON

data Mood 
  = Indicative
  | Imperative -- just drop the you in an otherwise valid indicative second person sentence
--  | Potential
--  | Interrogative -- not relevant rn
--  | Conditional -- also not relevant rn
--  | Subjunctive -- also not relevant rn
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Mood where
  parseJSON = genericParseJSON
instance ToJSON Mood where
  toJSON = genericToJSON

allMoods :: [Mood]
allMoods = allValues

-- no need for future tense rn, cuts down on the size of things for now.
data SimpleTense = Past | Present -- | Future
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON SimpleTense where
  parseJSON = genericParseJSON
instance ToJSON SimpleTense where
  toJSON = genericToJSON

allSimpleTenses :: [SimpleTense]
allSimpleTenses = allValues

data Tense = Tense
  { simpleTense :: SimpleTense 
  , isPerfect :: Bool -- auxiliary verb have + pp
  , isContinuous :: Bool -- auxiliary verb + ger
  }
  deriving (Show, Read, Eq, Ord, Generic)

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

instance FromJSON Tense where
  parseJSON = Aes.withText "Tense" $ \str ->
    case (stringToTense (Text.unpack str)) of
      Just res -> return res
      Nothing -> Aes.typeMismatch "Tense" (Y.String str)
  --parseJSON invalid = typeMismatch "Tense" invalid
  --parseJSON = genericParseJSON
instance ToJSON Tense where
  toJSON = Y.String . Text.pack . tenseToString
  --toJSON = genericToJSON

allTenses :: [Tense]
allTenses = Tense <$> allSimpleTenses <*> allValues <*> allValues

-- no need for Passive voice rn
data Voice = ActiveVoice -- | PassiveVoice -- auxiliary verb be + pp
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Voice where
  parseJSON = genericParseJSON
instance ToJSON Voice where
  toJSON = genericToJSON

allVoices :: [Voice]
allVoices = allValues

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

verbTypeOptions :: MyOptions VerbType
verbTypeOptions  
  = (defOptions :: MyOptions VerbType)
      { nullaries =
          [ 
          ]
      , fieldDictionary = HM.fromList
          [ 
          ]
      , fieldPrefix = "verb"
      }

instance FromJSON VerbType where
  parseJSON = genericParseJSONWithOpts verbTypeOptions
instance ToJSON VerbType where
  toJSON = genericToJSONWithOpts verbTypeOptions

data PronounBaseType
  = PronounBase Person Specificity [NumberGender]
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON PronounBaseType where
  parseJSON = genericParseJSON
instance ToJSON PronounBaseType where
  toJSON = genericToJSON

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

posTypeOptions :: MyOptions POSType
posTypeOptions  
  = (defOptions :: MyOptions POSType)
      { nullaries =
          [ AdverbT
          , PrepositionT
          ]
      , constructorDictionary = HM.fromList
          [ ("NounT", "Noun")
          , ("PronounT", "Pronoun")
          , ("AdjectiveT", "Adjective")
          , ("VerbT", "Verb")
          , ("AdverbT", "Adverb")
          , ("PrepositionT", "Preposition")
          ]
      }

--data NounType = Nou
instance FromJSON POSType where
  parseJSON = genericParseJSONWithOpts posTypeOptions
instance ToJSON POSType where
  toJSON = genericToJSONWithOpts posTypeOptions

data SimpleInflection
  = InflPlural -- for nouns
  | InflThirdPersonPresent
  | InflSimplePast
  | InflPastParticiple
  | InflPresentParticiple
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

regularSimpleInflection :: SimpleInflection -> String -> String
regularSimpleInflection InflPlural = regularPlural
regularSimpleInflection InflThirdPersonPresent = addEss
regularSimpleInflection InflSimplePast = regularPast
regularSimpleInflection InflPastParticiple = regularPastParticiple
regularSimpleInflection InflPresentParticiple = regularPresentParticiple

instance Hashable SimpleInflection

simpleInflectionOptions :: MyOptions SimpleInflection
simpleInflectionOptions  
  = (defOptions :: MyOptions SimpleInflection)
      { nullaries = [] -- they are all nullaries, so this can be omitted
      , constructorDictionary = HM.fromList
          [ ("SimplePast", "Past")
          ]
      , constructorPrefix = "Infl"
      }

instance FromJSON SimpleInflection where
  parseJSON = genericParseJSONWithOpts simpleInflectionOptions
instance ToJSON SimpleInflection where
  toJSON = genericToJSONWithOpts simpleInflectionOptions
instance ToJSONKey SimpleInflection where
  toJSONKey = 
    Aes.toJSONKeyText (Text.pack . (Aes.constructorTagModifier $ getOptions simpleInflectionOptions) . show)
instance FromJSONKey SimpleInflection where
  fromJSONKey = Aes.FromJSONKeyTextParser (parseJSON . Y.String)


data Irregularities 
  = IrregularCopula
  -- | IrregularHave
  | IrregularSimple (Map SimpleInflection String)
  deriving (Show, Read, Eq, Ord, Generic)

simpleInflect :: String -> Maybe Irregularities -> SimpleInflection -> String
simpleInflect _ (Just IrregularCopula) InflPlural = "" -- just don't be stupid xD That's a bad note huh TODO fix lol
simpleInflect _ (Just IrregularCopula) InflThirdPersonPresent = "is"
simpleInflect _ (Just IrregularCopula) InflSimplePast = "was/were" -- technically this doesn't exist, which is why it
-- isn't a proper word xD lol fix TODO again I guess
-- this really shouldn't be called on the copula, but I'm defining it mostly for convenience in
-- 1) the type signature, and 
-- 2) getting the participles, which are the same across persons
simpleInflect _ (Just IrregularCopula) InflPastParticiple = "been"
simpleInflect _ (Just IrregularCopula) InflPresentParticiple = "being"
simpleInflect str (Just (IrregularSimple map)) infl =
  fromMaybe (regularSimpleInflection infl str) (M.lookup infl map)
simpleInflect str Nothing infl = regularSimpleInflection infl str
  


irregularitiesOptions :: MyOptions Irregularities
irregularitiesOptions  
  = (defOptions :: MyOptions Irregularities)
      { nullaries = 
          [ IrregularCopula
          -- , IrregularHave
          ]
      , constructorDictionary = HM.fromList
          [ 
          ]
      , constructorPrefix = "Irregular"
      , furtherOptions =
          (furtherOptions (defOptions :: MyOptions Irregularities)){Aes.sumEncoding = Aes.UntaggedValue}
      }

instance FromJSON Irregularities where
  parseJSON = genericParseJSONWithOpts irregularitiesOptions
instance ToJSON Irregularities where
  toJSON = genericToJSONWithOpts irregularitiesOptions

instance Monoid Irregularities where
  mempty = IrregularSimple mempty
  mappend (IrregularSimple map1) (IrregularSimple map2) = IrregularSimple (map1 <> map2)
  mappend _ _ = IrregularCopula
  

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

instance FromJSON POS where
  parseJSON = genericParseJSON
instance ToJSON POS where
  toJSON = genericToJSON
instance FromJSONKey POS
instance ToJSONKey POS

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

addEss :: String -> String
addEss word = reverse . addEssHelper $ reverse word
  where
    addEssHelper wb@('x':_) = "se"++wb
    addEssHelper wb@('s':_) = "se"++wb
    addEssHelper wb@('h':'c':_) = "se"++wb
    addEssHelper wb@('h':'s':_) = "se"++wb
    addEssHelper wb1@('y':l:wb) 
      = if isConsonant l
        then "sei"++(l:wb)
        else 's':wb1
    -- default rule
    addEssHelper wb = 's':wb

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
    regPluralHelper noun = addEss noun


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

-- validator
-- TODO implement
compatible :: POSType -> POS -> Bool
compatible _ _ = True

simpleInflectBE :: BaseEntry -> SimpleInflection -> String
simpleInflectBE BaseEntry{baseText = str, irregularities = irr} = simpleInflect str irr

-- takes a dictionary form word, its POS info, the desired inflection's form, and produces the inflected form.
-- returns Nothing if the POS and POSType are incompatible
-- doesn't check irregulars obvi
inflectString :: BaseEntry -> POS -> Maybe String
inflectString
  be@BaseEntry
    { partOfSpeechType = (NounT (SimpleNounBase Count _))
    }
  (Noun (SimpleNoun Plural)) = 
    Just $ simpleInflectBE be InflPlural
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Adjective PresentParticiple) =
    Just $ simpleInflectBE be InflPresentParticiple
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Adjective PastParticiple) =
    Just $ simpleInflectBE be InflPastParticiple
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Present False False) ActiveVoice Third (Singular _))) =
    Just $ simpleInflectBE be InflThirdPersonPresent
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Present False False) ActiveVoice Third NotCount)) =
    Just $ simpleInflectBE be InflThirdPersonPresent
inflectString
  be@BaseEntry
    { partOfSpeechType = (VerbT _)
    }
  (Verb (SimpleVerb Indicative (Tense Past False False) ActiveVoice _ _)) =
    Just $ simpleInflectBE be InflSimplePast
inflectString
  be@BaseEntry
    { baseText = verb
    , partOfSpeechType = (VerbT _)
    , irregularities = irr
    }
  (Verb (SimpleVerb Indicative t@Tense{isContinuous=True} ActiveVoice p ng)) = do
    helper <- inflectString copulaBase (Verb (SimpleVerb Indicative t{isContinuous=False} ActiveVoice p ng))
    let prespart = simpleInflect verb irr InflPresentParticiple
    return $ helper ++ " " ++ prespart
inflectString
  be@BaseEntry
    { baseText = verb
    , partOfSpeechType = (VerbT _)
    , irregularities = irr
    }
  (Verb (SimpleVerb Indicative t@Tense{isPerfect=True} ActiveVoice p ng)) = do
    helper <- inflectString helperHaveBase (Verb (SimpleVerb Indicative t{isPerfect=False} ActiveVoice p ng))
    let pastpart = simpleInflect verb irr InflPastParticiple
    return $ helper ++ " " ++ pastpart
-- most inflections in English are trivial,
-- so the default case will just check if they're compatible and return true if they are.
inflectString be pos = if compatible (partOfSpeechType be) pos then Just (baseText be) else Nothing


inflect :: BaseEntry -> POS -> Maybe InflectedEntry
inflect be pos = do
    text <- (inflectString be pos)
    return $ InflectedEntry text pos be


data BaseEntry = BaseEntry
  { baseText :: String
  , partOfSpeechType :: POSType
  , irregularities :: Maybe Irregularities
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON BaseEntry where
  parseJSON = genericParseJSON
instance ToJSON BaseEntry where
  toJSON = genericToJSON

pastParticiplePOS :: POS
pastParticiplePOS = Adjective PastParticiple

simplePastTense :: Tense
simplePastTense = Tense Past False False

simplePresentTense :: Tense
simplePresentTense = Tense Present False False

simplePastPOS :: Person -> NumberGender -> POS
simplePastPOS p ng = Verb $ SimpleVerb Indicative simplePastTense ActiveVoice p ng

simplePresentPOS :: Person -> NumberGender -> POS
simplePresentPOS p ng = Verb $ SimpleVerb Indicative simplePresentTense ActiveVoice p ng

regularTestBase :: BaseEntry
regularTestBase = BaseEntry
  { baseText = "work"
  , partOfSpeechType = (VerbT (SimpleVerbBase Intransitive))
  , irregularities = Nothing
  }

simpleIrregularBase :: BaseEntry
simpleIrregularBase = BaseEntry
  { baseText = "eat"
  , partOfSpeechType = (VerbT (SimpleVerbBase Transitive))
  , irregularities = (irregularSimplePast "ate") <> (irregularPastParticiple "eaten")
  }

irregularThirdPersonPresent :: String -> Maybe Irregularities
irregularThirdPersonPresent = Just . IrregularSimple . M.singleton InflThirdPersonPresent
  
  --singular <- [NotCount] <|> Singular <$> allGenders
  --return (, str)
  

irregularSimplePast :: String -> Maybe Irregularities
irregularSimplePast = Just . IrregularSimple . M.singleton InflSimplePast
  --ng <- anyNumGen
  --person <- allPersons
  --return (simplePastPOS person ng, str)


irregularPastParticiple :: String -> Maybe Irregularities
irregularPastParticiple = Just . IrregularSimple . M.singleton InflPastParticiple
--irregularPastParticiple str = 
  --[ (Adjective PastParticiple, str)
  --]


copulaBase :: BaseEntry
copulaBase = BaseEntry
  { baseText = "be"
  , partOfSpeechType = (VerbT Copula)
  , irregularities = Just IrregularCopula
  {-
  , irregularForms = M.fromList $ 
      [ (Adjective PastParticiple, "been")
      ] <|>
      [ (simplePresentPOS First Plural, "are")
      , (simplePresentPOS Second Plural, "are")
      , (simplePresentPOS Third Plural, "are")
      , (simplePastPOS First Plural, "were")
      , (simplePastPOS Second Plural, "were")
      , (simplePastPOS Third Plural, "were")
      ] <|>
      ( do
          singular <- (Singular <$> allGenders) <|> [NotCount]
          ( [ (simplePresentPOS First singular, "am")
            , (simplePresentPOS Second singular, "are")
            , (simplePresentPOS Third singular, "is")
            , (simplePastPOS First singular, "was")
            , (simplePastPOS Second singular, "were")
            , (simplePastPOS Third singular, "was")
            ])
      )
      -}
  }

helperHaveBase :: BaseEntry
helperHaveBase = BaseEntry
  { baseText = "have"
  , partOfSpeechType = (VerbT HelperVerb)
  , irregularities = 
      (irregularSimplePast "had") <> (irregularPastParticiple "had") <> (irregularThirdPersonPresent "has")
  }

generateEntries :: BaseEntry -> [InflectedEntry]
generateEntries be = mapMaybe (inflect be) . possibleInflectedPOSs $ partOfSpeechType be

-- 
data InflectedEntry = InflectedEntry
  { entryText :: String
  , partOfSpeech :: POS
  , baseForm :: BaseEntry
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON InflectedEntry where
  parseJSON = genericParseJSON
instance ToJSON InflectedEntry where
  toJSON = genericToJSON

-- takes a base entry, wordText, position
data WordEntry = WordEntry
  { wordText :: String
  , baseInflection :: InflectedEntry 
  , wordPosition :: Maybe (Int, Int)
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON WordEntry where
  parseJSON = genericParseJSON
instance ToJSON WordEntry where
  toJSON = genericToJSON

generateWords :: InflectedEntry -> [WordEntry]
generateWords inflEnt =
  let
    ws = words (entryText inflEnt)
    lw = length ws
  in case ws of 
    [oneWord] -> 
      [WordEntry oneWord inflEnt Nothing]
    _ ->
      map (\(singWord, pos) -> WordEntry singWord inflEnt (Just (pos,lw)))
        $ zip ws [1..]

generateDictionaryList :: [BaseEntry] -> [WordEntry]
generateDictionaryList bes = bes >>= generateEntries >>= generateWords

generateDictionary :: [BaseEntry] -> HashMap String [WordEntry]
generateDictionary bes = HM.fromListWith (++) (map (\we -> (wordText we, [we])) (generateDictionaryList bes))

getWordEntries :: (HashMap String [WordEntry]) -> String -> [WordEntry]
getWordEntries dict = fromMaybe [] . flip HM.lookup dict




