--
-- EnglishWords.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module English.Words
  ( verbWorkBase
  , verbEatBase
  , copulaBase
  , helperHaveBase
  , regularCountNouns
  , regularTransitiveVerbs
  , regularIntransitiveVerbs
  , irregularTransitiveVerbs
  , irregularIntransitiveVerbs
  , irregularDitransitiveVerbs
  , wordDictionary
  ) where

import Dictionary
import Dictionary.Types
import Dictionary.Utils

import Control.Applicative ((<$>))
import Data.Monoid ((<>))

import qualified Data.Map as M

intransitiveVerbT :: POSType
intransitiveVerbT = VerbT (SimpleVerbBase Intransitive)

transitiveVerbT :: POSType
transitiveVerbT = VerbT (SimpleVerbBase Intransitive)

countNounT :: POSType
countNounT = NounT (SimpleNounBase Count [Singular Neuter, Plural])

verbWorkBase :: BaseEntry
verbWorkBase = BaseEntry
  { baseText = "work"
  , partOfSpeechType = intransitiveVerbT
  , irregularities = Nothing
  }


verbEatBase :: BaseEntry
verbEatBase = BaseEntry
  { baseText = "eat"
  , partOfSpeechType = transitiveVerbT
  , irregularities = (irregularSimplePast "ate") <> (irregularPastParticiple "eaten")
  }

nounAppleBase :: BaseEntry
nounAppleBase = BaseEntry
  { baseText = "apple"
  , partOfSpeechType = countNounT
  , irregularities = Nothing
  }

nounTableBase :: BaseEntry
nounTableBase = BaseEntry
  { baseText = "table"
  , partOfSpeechType = countNounT
  , irregularities = Nothing
  }

regularCountNouns :: [BaseEntry]
regularCountNouns = BaseEntry
  <$>
    [ "apple"
    , "desk"
    , "table"
    , "sword"
    , "lamp"
    , "candle"
    , "bed"
    , "pillow"
    , "door"
    , "ring"
    , "shirt"
    , "necklace"
    , "spear"
    , "steak"
    , "plate"
    , "knife"
    , "fork"
    ]
  <*> [ countNounT ]
  <*> [ Nothing ]


regularTransitiveVerbs :: [BaseEntry]
regularTransitiveVerbs = BaseEntry
  <$>
    [ "slice"
    , "help"
    ]
  <*> [ transitiveVerbT ]
  <*> [ Nothing ]

regularIntransitiveVerbs :: [BaseEntry]
regularIntransitiveVerbs = BaseEntry
  <$>
    [ "work"
    , "help"
    ]
  <*> [ intransitiveVerbT ]
  <*> [ Nothing ]

regularVerbs :: [BaseEntry]
regularVerbs = regularTransitiveVerbs ++ regularIntransitiveVerbs

irregularVerb :: (String, String, String) -> Transitivity -> BaseEntry
irregularVerb (base, past, pp) trans = BaseEntry base (VerbT (SimpleVerbBase trans)) . Just . IrregularSimple 
  $ M.fromList [(InflSimplePast, past), (InflPastParticiple, pp)]

irregularTransitiveVerbs :: [BaseEntry]
irregularTransitiveVerbs =
  flip irregularVerb Transitive <$>
    [ ("eat","ate","eaten")
    , ("take","took","taken")
    ]

irregularIntransitiveVerbs :: [BaseEntry]
irregularIntransitiveVerbs =
  flip irregularVerb Intransitive <$>
    [ ("eat","ate","eaten")
    , ("come","came","come")
    , ("go","went","gone")
    , ("sleep","slept","slept")
    ]

irregularDitransitiveVerbs :: [BaseEntry]
irregularDitransitiveVerbs =
  flip irregularVerb Ditransitive <$>
    [ ("buy","bought","bought")
    ]

irregularVerbs :: [BaseEntry]
irregularVerbs = irregularTransitiveVerbs ++ irregularIntransitiveVerbs ++ irregularDitransitiveVerbs

specifierThe :: BaseEntry
specifierThe = BaseEntry "the" (AdjectiveT (DeterminerBase Specific anyNumGen)) Nothing

specifierA :: BaseEntry
specifierA = BaseEntry "a" (AdjectiveT (DeterminerBase General (Singular <$> allGenders))) Nothing

specifierAn :: BaseEntry
specifierAn = BaseEntry "an" (AdjectiveT (DeterminerBase General (Singular <$> allGenders))) Nothing

simpleAdjectives :: [BaseEntry]
simpleAdjectives = BaseEntry 
  <$>
    [ "black"
    , "blue"
    , "red"
    , "metallic"
    , "shiny"
    , "silver"
    , "green"
    , "orange"
    , "small"
    , "big"
    , "soft"
    , "hard"
    , "sharp"
    , "dull"
    , "golden"
    , "gold"
    , "transparent"
    ]
  <*> [ AdjectiveT SimpleAdjectiveBase ]
  <*> [ Nothing ]

-- I ate the shiny red apple.
-- y y   y   y     y   y

pronounI :: BaseEntry
pronounI = BaseEntry "I" (PronounT $ PronounBase First Specific (Singular <$> allGenders)) Nothing 

pronounWe :: BaseEntry
pronounWe = BaseEntry "we" (PronounT $ PronounBase First Specific [Plural]) Nothing 

pronounYou :: BaseEntry
pronounYou =
  BaseEntry
    "you"
    (PronounT $ PronounBase Second Specific ([Plural] ++ (Singular <$> allGenders)))
    Nothing 

pronouns :: [BaseEntry]
pronouns = [pronounI, pronounWe, pronounYou]


wordDictionary :: [BaseEntry]
wordDictionary = regularCountNouns
  ++ regularVerbs
  ++ irregularVerbs
  ++  [ copulaBase
      ]
  ++ regularCountNouns
  ++  [ specifierThe
      , specifierA
      , specifierAn
      ]
  ++ simpleAdjectives
  ++ pronouns



