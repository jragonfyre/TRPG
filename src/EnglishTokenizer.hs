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

type Entry = String

type Dictionary = String -> [Entry]

data Token 
  = Word
    { pos :: SourcePos
    , text :: String
    , entries :: [Entry]
    }
  | Comma SourcePos
  | Semicolon SourcePos
  | Period SourcePos
  | QuestionMark SourcePos
  deriving (Show, Eq)

tokenText :: Token -> String
tokenText w@(Word _ _ _) = text w
tokenText (Comma _) = ","
tokenText (Semicolon _) = ";"
tokenText (Period _) = "."
tokenText (QuestionMark _) = "?"

isWord :: Token -> Bool
isWord (Word _ _ _) = True
isWord _ = False

tokenPos :: Token -> SourcePos
tokenPos w@(Word _ _ _) = pos w
tokenPos (Comma p) = p
tokenPos (Semicolon p) = p
tokenPos (Period p) = p
tokenPos (QuestionMark p) = p

tokenEntries :: Token -> [Entry]
tokenEntries w@(Word _ _ _) = entries w
tokenEntries _ = []


type Tokenizer = Parsec String () [Token]

-- parses a word from the input 
word :: Parsec String u (SourcePos, String)
word = do
  st <- getParserState
  res <- many1 (letter <|> (oneOf "'-"))
  return (statePos st, res)

wordToken :: Parsec String Dictionary Token
wordToken = do
  dict <- fmap stateUser $ getParserState
  fmap (\(pos, t) -> Word pos t (dict t)) word


punctuationToken :: Parsec String u Token
punctuationToken = do
  st <- getParserState
  ch <- oneOf ",;.?"
  let
    tmap = if | ch == ',' -> Comma
              | ch == ';' -> Semicolon
              | ch == '.' -> Period
              | ch == '?' -> QuestionMark
  return $ tmap $ statePos st

tokenRec :: Parsec String Dictionary Token
tokenRec = wordToken <|> punctuationToken

tokenize :: Parsec String Dictionary [Token]
tokenize = fmap join $ sepBy (many1 tokenRec) (space >> spaces)

makeTokens :: Dictionary -> String -> String -> Either ParseError [Token]
makeTokens dict srcname src = runP tokenize dict srcname src

