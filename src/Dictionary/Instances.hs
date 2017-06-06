--
-- Instances.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary.Instances
  (
  ) where

import Dictionary.Types
import Dictionary.Utils

import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey (..), FromJSONKey (..))
import qualified Data.Aeson as Aes
import qualified Data.Aeson.Types as Aes
import ParseUtilities

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), ToJSON (..))

instance FromJSON Gender where
  parseJSON = genericParseJSON
instance ToJSON Gender where
  toJSON = genericToJSON

instance FromJSON Countable where
  parseJSON = genericParseJSON
instance ToJSON Countable where
  toJSON = genericToJSON


instance FromJSON Person where
  parseJSON = genericParseJSON
instance ToJSON Person where
  toJSON = genericToJSON

instance FromJSON Case where
  parseJSON = genericParseJSON
instance ToJSON Case where
  toJSON = genericToJSON

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

instance FromJSON NounBaseType where
  parseJSON = genericParseJSON
instance ToJSON NounBaseType where
  --toJSON (SimpleNounBase count []) = 
  toJSON = genericToJSON


instance FromJSON NounType where
  parseJSON = genericParseJSON
instance ToJSON NounType where
  toJSON = genericToJSON

instance FromJSON AdjectiveBaseType where
  parseJSON = genericParseJSON
instance ToJSON AdjectiveBaseType where
  toJSON = genericToJSON


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

instance FromJSON Specificity where
  parseJSON = genericParseJSON
instance ToJSON Specificity where
  toJSON = genericToJSON

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


instance FromJSON Transitivity where
  parseJSON = genericParseJSON
instance ToJSON Transitivity where
  toJSON = genericToJSON

instance FromJSON Mood where
  parseJSON = genericParseJSON
instance ToJSON Mood where
  toJSON = genericToJSON

instance FromJSON SimpleTense where
  parseJSON = genericParseJSON
instance ToJSON SimpleTense where
  toJSON = genericToJSON

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
 
instance FromJSON Voice where
  parseJSON = genericParseJSON
instance ToJSON Voice where
  toJSON = genericToJSON

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

instance FromJSON PronounBaseType where
  parseJSON = genericParseJSON
instance ToJSON PronounBaseType where
  toJSON = genericToJSON

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
  
instance FromJSON POS where
  parseJSON = genericParseJSON
instance ToJSON POS where
  toJSON = genericToJSON
instance FromJSONKey POS
instance ToJSONKey POS

instance FromJSON BaseEntry where
  parseJSON = genericParseJSON
instance ToJSON BaseEntry where
  toJSON = genericToJSON

instance FromJSON InflectedEntry where
  parseJSON = genericParseJSON
instance ToJSON InflectedEntry where
  toJSON = genericToJSON

instance FromJSON WordEntry where
  parseJSON = genericParseJSON
instance ToJSON WordEntry where
  toJSON = genericToJSON






