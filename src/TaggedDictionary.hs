--
-- TaggedDictionary.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module TaggedDictionary where
--  (
--  ) where

import GHC.Generics

data PosTag = Noun | Verb | Pronoun | Adjective | Adverb | Preposition | Clause
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data Inflection :: PosTag -> PosTag -> * where
  PastParticiple :: Inflection Verb Adjective
  PresentParticiple :: Inflection Verb Adjective
  Verbal :: Inflection Verb Verb
  -- constructors to follow

deriving instance Eq (Inflection a b)
--deriving instance Ord (Inflection a b)
deriving instance Show (Inflection a b)
--deriving instance Read (Inflection a b)
--deriving instance Generic (Inflection a b)

data Entry :: PosTag -> * where
  EntryNoun :: 
    { entryNoun :: String
    , entryPlural :: Maybe String -- irregular plural if there is one
    } -> Entry Noun
  EntryCopula :: Entry Verb -- ... uh the copula I guess
  EntryVerb ::
    { entryVerb :: String
    , entryPast :: Maybe String
    , entryPastParticiple :: Maybe String
    , entryPresentParticiple :: Maybe String
    , entryThirdPersonPresentSingular :: Maybe String
    } -> Entry Verb

deriving instance Eq (Entry a)
--deriving instance Ord (Entry a)
deriving instance Show (Entry a)
--deriving instance Read (Entry a)
--deriving instance Generic (Entry a)

entryText :: Entry a -> String
entryText EntryNoun{entryNoun=txt} = txt
entryText EntryCopula = "be"
entryText EntryVerb{entryVerb=txt} = txt



inflect :: Entry a -> Inflection a b -> Inflected b
inflect e _ = FromString $ entryText e

data Inflected :: PosTag -> * where
  FromString :: String -> Inflected a

deriving instance Eq (Inflected a)
--deriving instance Ord (Inflected a)
deriving instance Show (Inflected a)
--deriving instance Read (Inflected a)
--deriving instance Generic (Inflected a)

data SingleWord :: PosTag -> * where

deriving instance Eq (SingleWord a)
--deriving instance Ord (SingleWord a)
deriving instance Show (SingleWord a)
--deriving instance Read (SingleWord a)

class Modifier a where

instance Modifier Adjective
instance Modifier Adverb

data Expression :: PosTag -> * where
  -- atoms should have meaning, so they are inflected forms, not words
  ExpAtom :: Inflected a -> Expression a
  ExpPossessive :: Expression Noun -> Expression Adjective
  ExpClause :: Expression Noun -> Expression Verb -> Expression Clause
  ExpPrepositionalPhrase :: (Modifier a) => Expression Preposition -> Expression Noun -> Expression a
  -- nominals have prefix and postfix modifiers
  ExpNominal :: [Expression Adjective] -> Expression Noun -> [Expression Adjective] -> Expression Noun
  ExpVerbal :: [Expression Adverb] -> Expression Verb -> [Expression Adverb] -> Expression Verb

deriving instance Eq (Expression a)
--deriving instance Ord (Expression a)
deriving instance Show (Expression a)
--deriving instance Read (Expression a)


