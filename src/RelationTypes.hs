--
-- RelationTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module RelationTypes where

import qualified Data.HashMap.Lazy as M
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), (.:), (.!=), (.:?), (.=), ToJSON (..))
import Control.Applicative ((<$>),(<*>),(<|>))
import qualified Data.Maybe as May
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson.Types (typeMismatch)
import Data.Vector (toList)

import BaseTypes
import UtilityParsers


data Relation = Relation
  { relationName :: Identifier
  , relationRequires :: [Condition]
  , relationDescriptions :: [String]
  , relationProperties :: [RelationProperty]
  }
  deriving (Show, Read, Eq)

instance ToJSON Relation where
  toJSON rel = Y.object
    [ "relation" .= relationName rel
    , "requires" .= toJSONList (relationRequires rel)
    , "descriptions" .= toJSONList (relationDescriptions rel)
    , "properties" .= toJSONList (relationProperties rel)
    ]

instance FromJSON Relation where
  parseJSON (Y.Object v) = Relation 
    <$> v .: "relation"
    <*> v .:? "requires" .!= []
    <*> v .:? "descriptions" .!= []
    <*> v .:? "properties" .!= []
  parseJSON invalid = typeMismatch "Relation" invalid
    

type Reason = Identifier
type Result = Relation

data RelationProperty
  = Portal -- subspace or portal
  | Visible Visibility
  | Unstable Reason Result
  deriving (Show, Read, Eq)

instance ToJSON RelationProperty where
  toJSON Portal = Y.String "portal"
  toJSON (Visible vis) = Y.object ["visible" .= (toJSON vis)]
  toJSON (Unstable reas res) 
    = Y.object ["unstable" .= (Y.object ["reason" .= (toJSON reas), "result" .= (toJSON res)])]

instance FromJSON RelationProperty where
  parseJSON (Y.String "portal") = return Portal
  parseJSON (Y.Object v) 
    =   isKVPair v "visible" Visible
    <|> do 
          unstable <- v .: "unstable"
          reas <- unstable .: "reason"
          res <- unstable .: "result"
          return (Unstable reas res)
    <|> typeMismatch "RelationProperty" (Y.Object v)
  parseJSON invalid = typeMismatch "RelationProperty" invalid

  parseJSONList arr = parseAMAP arr parseJSON
 

