--
-- EnglishTokenizer.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module English.Tokenizer where
--  (
--  ) where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char

import qualified Data.List.NonEmpty as NE

import Control.Monad (join)
import Control.Monad.Reader


-- takes a string representing a word and produces
-- a collection of dictionary entries corresponding to that
-- word.
type Dictionary a = String -> [a]

-- MASSIVE problem: currently no support for compound words. English is filled with these, like "pick up"

data EngToken a
  = Word
    { pos :: SourcePos
    , text :: String
    , entries :: [a]
    }
  | Comma SourcePos
  | Semicolon SourcePos
  | Period SourcePos
  | QuestionMark SourcePos
  deriving (Show, Eq)--, Generic)

tokenText :: EngToken a -> String
tokenText w@(Word _ _ _) = text w
tokenText (Comma _) = ","
tokenText (Semicolon _) = ";"
tokenText (Period _) = "."
tokenText (QuestionMark _) = "?"

isWord :: EngToken a -> Bool
isWord (Word _ _ _) = True
isWord _ = False

tokenPos :: EngToken a -> SourcePos
tokenPos w@(Word _ _ _) = pos w
tokenPos (Comma p) = p
tokenPos (Semicolon p) = p
tokenPos (Period p) = p
tokenPos (QuestionMark p) = p

tokenEntries :: EngToken a -> [a]
tokenEntries w@(Word _ _ _) = entries w
tokenEntries _ = []


-- parses a word from the input 
word :: (ErrorComponent e, Monad m) => ParsecT e String m (SourcePos, String)
word = do
  st <- getParserState
  res <- some (letterChar <|> (oneOf ("'-"::[Char])))
  return (NE.head $ statePos st, res)

wordToken :: (ErrorComponent e) => ParsecT e String (Reader (Dictionary a)) (EngToken a)
wordToken = do
  dict <- ask
  fmap (\(pos, t) -> Word pos t (dict t)) word


punctuationToken :: (ErrorComponent e) => ParsecT e String (Reader (Dictionary a)) (EngToken a)
punctuationToken = do
  st <- getParserState
  ch <- oneOf (",;.?"::[Char]) <?> "punctuation"
  let
    tmap = if | ch == ',' -> Comma
              | ch == ';' -> Semicolon
              | ch == '.' -> Period
              | ch == '?' -> QuestionMark
  return . tmap . NE.head $ statePos st

tokenRec :: (ErrorComponent e) => ParsecT e String (Reader (Dictionary a)) (EngToken a)
tokenRec = wordToken <|> punctuationToken

tokenize :: (ErrorComponent e) => ParsecT e String (Reader (Dictionary a)) [EngToken a]
tokenize = fmap join $ sepBy (some tokenRec) (some spaceChar)

makeTokens :: Dictionary a -> String -> String -> Either (ParseError Char Dec) [EngToken a]
makeTokens dict srcname src = flip runReader dict $ runParserT tokenize srcname src


