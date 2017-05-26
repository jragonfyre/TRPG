--
-- EnglishTokenizer.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module EnglishTokenizer where
--  (
--  ) where

import Text.Parsec
import Control.Monad (join)


-- takes a string representing a word and produces
-- a collection of dictionary entries corresponding to that
-- word.
type Dictionary a = String -> [a]

-- MASSIVE problem: currently no support for compound words. English is filled with these, like "pick up"

data Token a
  = Word
    { pos :: SourcePos
    , text :: String
    , entries :: [a]
    }
  | Comma SourcePos
  | Semicolon SourcePos
  | Period SourcePos
  | QuestionMark SourcePos
  deriving (Show, Eq)

tokenText :: Token a -> String
tokenText w@(Word _ _ _) = text w
tokenText (Comma _) = ","
tokenText (Semicolon _) = ";"
tokenText (Period _) = "."
tokenText (QuestionMark _) = "?"

isWord :: Token a -> Bool
isWord (Word _ _ _) = True
isWord _ = False

tokenPos :: Token a -> SourcePos
tokenPos w@(Word _ _ _) = pos w
tokenPos (Comma p) = p
tokenPos (Semicolon p) = p
tokenPos (Period p) = p
tokenPos (QuestionMark p) = p

tokenEntries :: Token a -> [a]
tokenEntries w@(Word _ _ _) = entries w
tokenEntries _ = []


-- parses a word from the input 
word :: Parsec String u (SourcePos, String)
word = do
  st <- getParserState
  res <- many1 (letter <|> (oneOf "'-"))
  return (statePos st, res)

wordToken :: Parsec String (Dictionary a) (Token a)
wordToken = do
  dict <- fmap stateUser $ getParserState
  fmap (\(pos, t) -> Word pos t (dict t)) word


punctuationToken :: Parsec String u (Token a)
punctuationToken = do
  st <- getParserState
  ch <- oneOf ",;.?"
  let
    tmap = if | ch == ',' -> Comma
              | ch == ';' -> Semicolon
              | ch == '.' -> Period
              | ch == '?' -> QuestionMark
  return $ tmap $ statePos st

tokenRec :: Parsec String (Dictionary a) (Token a)
tokenRec = wordToken <|> punctuationToken

tokenize :: Parsec String (Dictionary a) [Token a]
tokenize = fmap join $ sepBy (many1 tokenRec) (space >> spaces)

makeTokens :: Dictionary a -> String -> String -> Either ParseError [Token a]
makeTokens dict srcname src = runP tokenize dict srcname src


