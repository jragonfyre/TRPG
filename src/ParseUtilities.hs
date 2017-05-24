--
-- UtilityParsers.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module ParseUtilities
  ( isKVPair
  , parseAMAP
  ) where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), (.:), (.:?), (.!=))
import Data.Text (Text)



isKVPair :: FromJSON a => Y.Object -> Text -> (a -> b) -> Y.Parser b
isKVPair obj txt f = fmap f (obj .: txt)


  --parseJSONList :: Y.Value -> Y.Parser [Property]
parseAMAP :: Y.Value -> (Y.Value -> Y.Parser a) -> Y.Parser [a]
parseAMAP (Y.Array arr) pJSON = parseAMAPHelper (toList arr)
  where
    parseAMAPHelper [] = return []
    parseAMAPHelper (a:xs) = do
      rest <- parseJSONListHelper xs
      ( do
          ar <- pJSON a
          return (ar:rest)) <|> (return rest)


