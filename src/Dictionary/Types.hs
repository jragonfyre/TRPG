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
