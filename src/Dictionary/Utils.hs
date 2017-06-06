--
-- Utils.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Dictionary.Utils
  ( tenseToString
  , stringToTense
  ) where

import Dictionary.Types

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

