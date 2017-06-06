--
-- ObjectTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Game.ObjectTypes where
--  (
--  ) where

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

import Game.BaseTypes
import qualified Game.RelationTypes as Rel
import Game.RelationTypes (Relation)
import ParseUtilities

data ObjectProperty
  = Visible Visibility
  | Durability Int
  | Value Int
  | Material Identifier
  deriving (Show, Read, Eq)

instance ToJSON ObjectProperty where
  --toJSON Flammable = Y.String "flammable"
  --toJSON Metallic = Y.String "metallic"
  --toJSON Wooden = Y.String "wooden"
  --toJSON Carvable = Y.String "carvable"
  toJSON (Durability dur) = Y.object ["durability" .= (toJSON dur)]
  toJSON (Value val) = Y.object ["value" .= (toJSON val)]
  toJSON (Visible vis) = Y.object ["visible" .= (toJSON vis)]
  toJSON (Material id) = Y.object ["material" .= (toJSON id)]

instance FromJSON ObjectProperty where
  --parseJSON (Y.String "flammable") = return Flammable
  --parseJSON (Y.String "metallic") = return Metallic
  --parseJSON (Y.String "wooden") = return Wooden
  --parseJSON (Y.String "carvable") = return Carvable
  parseJSON (Y.Object v) 
    =   isKVPair v "durability" Durability
    <|> isKVPair v "value" Value
    <|> isKVPair v "visible" Visible
    <|> isKVPair v "material" Material
    <|> typeMismatch "ObjectProperty" (Y.Object v)
  parseJSON invalid = typeMismatch "ObjectProperty" invalid

  parseJSONList = parseAMAP parseJSON

type ObjectClass = ObjectClassSt Identifier

data ObjectClassSt a =
  ObClassSt
    { objClassName :: Identifier
    , objClassProperties :: [ObjectProperty]
    , objClassRelationships :: [a]
    }
  deriving (Show, Read, Eq)

instance (ToJSON a) => ToJSON (ObjectClassSt a) where
  toJSON obclass = Y.object
    [ "object-class" .= objClassName obclass
    , "properties" .= toJSONList (objClassProperties obclass)
    , "relations" .= toJSONList (objClassRelationships obclass)
    ]

instance (FromJSON a) => FromJSON (ObjectClassSt a) where
  parseJSON (Y.Object v) = ObClassSt
    <$> v .: "object-class"
    <*> v .:? "properties" .!= []
    <*> v .:? "relations" .!= []
  parseJSON invalid = typeMismatch "ObjectClass" invalid


