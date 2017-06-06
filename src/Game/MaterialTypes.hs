--
-- MaterialTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Game.MaterialTypes where
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
import ParseUtilities

type Material = MaterialSt Identifier

data MaterialSt a = MaterialSt
  { materialName :: Identifier
  , materialParents :: [a]
  , materialDescriptions :: [String]
  , materialProperties :: [MaterialProperty]
  }
  deriving (Show, Eq, Read)

instance (ToJSON a) => ToJSON (MaterialSt a) where
  toJSON mat = Y.object
    [ "material" .= materialName mat
    , "parents" .= toJSONList (materialParents mat)
    , "descriptions" .= toJSONList (materialDescriptions mat)
    , "properties" .= toJSONList (materialProperties mat)
    ]

-- that way we can parse to Material Identifier, then later go to Material Material
instance (FromJSON a) => FromJSON (MaterialSt a) where
  parseJSON (Y.Object v) = MaterialSt
    <$> v .: "material"
    <*> v .:? "parents" .!= []
    <*> v .:? "descriptions" .!= []
    <*> v .:? "properties" .!= []
  parseJSON invalid = typeMismatch "Material" invalid
    


data MaterialProperty
  = Flammable
  | Metallic
  | Wooden
  | Carvable
  | Durability Integer
  | Value Integer
  deriving (Show, Read, Eq)

instance ToJSON MaterialProperty where
  toJSON Flammable = Y.String "flammable"
  toJSON Metallic = Y.String "metallic"
  toJSON Wooden = Y.String "wooden"
  toJSON Carvable = Y.String "carvable"
  toJSON (Durability dur) = Y.object ["durability" .= (toJSON dur)]
  toJSON (Value val) = Y.object ["value" .= (toJSON val)]

instance FromJSON MaterialProperty where
  parseJSON (Y.String "flammable") = return Flammable
  parseJSON (Y.String "metallic") = return Metallic
  parseJSON (Y.String "wooden") = return Wooden
  parseJSON (Y.String "carvable") = return Carvable
  parseJSON (Y.Object v) 
    =   isKVPair v "durability" Durability
    <|> isKVPair v "value" Value
    <|> typeMismatch "MaterialProperty" (Y.Object v)
  parseJSON invalid = typeMismatch "MaterialProperty" invalid

  parseJSONList = parseAMAP parseJSON



