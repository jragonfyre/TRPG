--
-- EnglishParser.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module EnglishParser where
--  (
--  ) where

import Text.Megaparsec.Prim
import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos (SourcePos)
import Text.Megaparsec.Error (Dec, ParseError, ErrorItem (..))

import Control.Monad

import EnglishTokenizer
import Dictionary
import GHC.Generics

import Data.Functor.Identity

import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.HashMap.Lazy as HM

import EnglishWords (wordDictionary)

type SimpleToken a = (SourcePos, a)

type WordToken = SimpleToken WordEntry
type InflectedToken = SimpleToken InflectedEntry
--data InflectedToken = InflToken
--  { inflection = InflectedEntry
--  , sourcePos = SourcePos
--  }


toWordTokens :: EngToken WordEntry -> [WordToken]
toWordTokens engToken = map ((,) (tokenPos engToken)) $ tokenEntries engToken

baseDictionary :: HM.HashMap String [WordEntry]
baseDictionary = generateDictionary wordDictionary

baseTokenizer :: String -> String -> Either (ParseError Char Dec) [EngToken WordEntry]
baseTokenizer = makeTokens (getWordEntries baseDictionary) 

testTokenizer :: String -> Either (ParseError Char Dec) [EngToken WordEntry]
testTokenizer = baseTokenizer "test"

inflectedStreams :: [EngToken WordEntry] -> [[InflectedToken]]
inflectedStreams = (sequence >=> (toInflectionStreamsGen snd (\(d,_) res -> (d,res)))) . map toWordTokens

testGetStreams :: String -> Either (ParseError Char Dec) [[InflectedToken]]
testGetStreams = fmap inflectedStreams . testTokenizer

{-
instance Ord (SimpleToken a) where
  compare (x,_) (y,_) = compare x y
-}

instance (Eq a, Ord a) => Stream [SimpleToken a] where
  type Token [SimpleToken a] = SimpleToken a

  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)

  updatePos _ _ _ (pos, _) = (pos, pos)

type WordStream = [InflectedToken]

type EnglishParser = ParsecT Dec WordStream Identity

satisfy :: (MonadParsec e s m, Token s ~ InflectedToken) => (InflectedEntry -> Bool) -> m InflectedEntry
satisfy f = token testT Nothing
  where
    testT x =
      if f (snd x)
      then Right (snd x)
      else Left (Set.singleton (Tokens (x:| [])), Set.empty, Set.empty)

simpleNoun :: EnglishParser (Expression NounTag)
simpleNoun = fmap ExpAtom (satisfy (isNoun))

-- 
--satisfy :: (MonadParsec e s m, Token s ~ WordEntry) => (InflectedEntry -> Bool) -> m InflectedEntry

data PosTag = NounTag | VerbTag | PronounTag | AdjectiveTag | AdverbTag | PrepositionTag | ClauseTag
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

class Modifier a where

instance Modifier AdjectiveTag
instance Modifier AdverbTag

class Nominal a where

instance Nominal NounTag
instance Nominal PronounTag

data Expression :: PosTag -> * where
  -- atoms should have meaning, so they are inflected forms, not words
  ExpAtom :: InflectedEntry -> Expression a
  ExpPossessive :: Expression NounTag -> Expression AdjectiveTag
  ExpClause :: Expression NounTag -> Expression VerbTag -> Expression ClauseTag
  ExpPrepositionalPhrase :: (Modifier a) => Expression PrepositionTag -> Expression NounTag -> Expression a
  -- nominals have prefix and postfix modifiers
  ExpNominal :: Maybe (Expression AdjectiveTag) -> [Expression AdjectiveTag] -> Expression NounTag -> [Expression AdjectiveTag] -> Expression NounTag
  ExpVerbal :: [Expression AdverbTag] -> Expression VerbTag -> [Expression AdverbTag] -> Expression VerbTag

deriving instance Eq (Expression a)
--deriving instance Ord (Expression a)
deriving instance Show (Expression a)
--deriving instance Read (Expression a)



