--
-- UtilityParsers.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module ParseUtilities where
{-
  ( isKVPair
  , parseAMAP
  , genericToJSON
  , genericParseJSON
  , genToJSONWithOpts
  , genParseJSONWithOpts
  , genericToJSONWithOpts
  , genericParseJSONWithOpts
  , defOptions
  , MyOptions (..)
  ) where
-}


import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), (.:), (.:?), (.!=))
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Data.Vector (toList)

import GHC.Generics
import Data.Aeson.Types (SumEncoding (ObjectWithSingleField), Options (..))
import Data.Aeson (defaultOptions)
import qualified Data.Aeson as DAes
import qualified Data.Aeson.Types as DAT
import Data.Aeson.Types (Parser, GToJSON, Value, Zero, GFromJSON, camelTo2)

import Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)

import Data.List as List

import Data.Maybe (fromMaybe)

--import Text.Read (readMaybe)

myDefaultOptions
  = defaultOptions
      { sumEncoding = ObjectWithSingleField
      , constructorTagModifier = camelTo2 '-'
      , fieldLabelModifier = camelTo2 '-'
      , omitNothingFields = False
      }

data MyOptions a = MyOptions
  { nullaries :: [a]
  , constructorDictionary :: HashMap String String
  , constructorPrefix :: String
  , fieldDictionary :: HashMap String String
  , fieldPrefix :: String
  , furtherOptions :: Options
  }

defOptions :: (Eq a) => MyOptions a
defOptions = MyOptions
  { nullaries = []
  , constructorDictionary = HM.empty
  , constructorPrefix = ""
  , fieldDictionary = HM.empty
  , fieldPrefix = ""
  , furtherOptions = myDefaultOptions
  }

optionalModifier :: (String -> Maybe String) -> String -> String
optionalModifier f str = fromMaybe str (f str)

deletePrefix :: String -> String -> String
deletePrefix prefix = optionalModifier (List.stripPrefix prefix)

getOptions :: MyOptions a -> Options
getOptions opts = 
  let
    fOpts = furtherOptions opts
    modCons = constructorTagModifier fOpts 
      . deletePrefix (constructorPrefix opts) 
      . optionalModifier (flip HM.lookup (constructorDictionary opts))
    modField = fieldLabelModifier fOpts 
      . deletePrefix (fieldPrefix opts)
      . optionalModifier (flip HM.lookup (fieldDictionary opts))
  in
    fOpts
      { constructorTagModifier = modCons
      , fieldLabelModifier = modField
      }


-- use for a type with some nullary constructors, but not all nullary
genToJSONWithOpts :: (Show a, Eq a, Generic a, GToJSON Zero (Rep a)) => MyOptions a -> a -> Value
genToJSONWithOpts opts v =
  let
    fOpts = getOptions opts
    displayCons = Text.pack . constructorTagModifier fOpts . show
  in 
    if v `elem` (nullaries opts)
    then Y.String $ displayCons v
    else DAes.genericToJSON fOpts v

genParseJSONWithOpts :: (Show a, Eq a, Generic a, GFromJSON Zero (Rep a)) => MyOptions a -> Value -> Parser a
genParseJSONWithOpts opts v@(Y.String str) =
  let
    fOpts = getOptions opts
    displayCons = Text.pack . constructorTagModifier fOpts . show
    nulls = nullaries opts
  in 
    case List.lookup str (zip (List.map displayCons nulls) nulls) of
      Just val -> return val
      Nothing -> DAes.genericParseJSON fOpts v
genParseJSONWithOpts opts v = 
  let
    fOpts = getOptions opts
    --displayCons = Text.pack . constructorTagModifier fOpts . show
    --nulls = nullaries opts
  in 
    DAes.genericParseJSON fOpts v

genericParseJSONWithOpts :: (Show a, Eq a, Generic a, GFromJSON Zero (Rep a))
    => MyOptions a -> Value -> Parser a
genericParseJSONWithOpts = genParseJSONWithOpts
genericToJSONWithOpts :: (Show a, Eq a, Generic a, GToJSON Zero (Rep a)) => MyOptions a -> a -> Value
genericToJSONWithOpts = genToJSONWithOpts

genericToJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
genericToJSON = DAes.genericToJSON myDefaultOptions

genericParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
genericParseJSON = DAes.genericParseJSON myDefaultOptions

isKVPair :: FromJSON a => Y.Object -> Text -> (a -> b) -> Y.Parser b
isKVPair obj txt f = fmap f (obj .: txt)


  --parseJSONList :: Y.Value -> Y.Parser [Property]
parseAMAP :: (Y.Value -> Y.Parser a) ->  Y.Value -> Y.Parser [a]
parseAMAP pJSON (Y.Array arr) = parseAMAPHelper (Data.Vector.toList arr)
  where
    parseAMAPHelper [] = return []
    parseAMAPHelper (a:xs) = do
      rest <- parseAMAPHelper xs
      ( do
          ar <- pJSON a
          return (ar:rest)) <|> (return rest)


