--
-- GrammarParser.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module GrammarParser where
--  (
--  ) where

-- a dictionary entry representing a single word which is a discrete noun, possibly with its irregular plural
data DiscreteNounEntry 
  = RegularN String           -- e.g. RegularN "house" or RegularN "box" 
  | IrregularN String String  -- e.g. IrregularN "mouse" "mice"
  deriving (Show, Read, Eq)

-- a dictionary entry for a mass noun, like mud
-- technically muds does make sense, like there were several muds on display at the mud store
-- similar to I bought several paints at the paint store, but paint is a mass noun
-- I guess also milk
-- complicated... will resolve later
type MassNounEntry = String

-- Dictionary entry for a proper noun, again plurals don't make sense, but they also aren't mass nouns, so...
type ProperNounEntry = String

-- dictionary entry for an adjective, like black, weak, near
type AdjectiveEntry = String

-- Is a discrete noun singular or plural
data NounQty = Singular | Plural
  deriving (Show, Read, Eq)

-- the kind for Noun, so that different types of nouns can be checked against each other and whatnot
data NounType = DiscreteT NounQty | MassNounT | ProperNounT
  deriving (Show, Read, Eq)

-- produces 
data Noun :: NounType -> * where
  SingularNoun :: DiscreteNounEntry -> Noun (DiscreteT Singular)
  PluralNoun :: DiscreteNounEntry -> Noun (DiscreteT Plural)
  MassNoun :: MassNounEntry -> Noun MassNounT
  ProperNoun :: ProperNounEntry -> Noun ProperNounT
  FromAdjective AdjectiveEntry -> Noun MassNounT
  deriving (Show, Read, Eq)

-- the kind for Adjectives
data AdjectiveType = Adjective | Possessive | Determiner NounType | Demonstrative NounType
  deriving (Show, Read, Eq)

data Adjective :: AdjectiveType -> 
  deriving (Show, Read, Eq)


data PhraseType
  = Nounlike
  | NounModifier
  | VerbModifier
  | Verbal

data Phrase a where
  NounP :: Determiner -> 












