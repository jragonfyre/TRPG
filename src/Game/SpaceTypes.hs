--
-- SpaceTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Game.SpaceTypes where
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

data SpaceProperty
  = Enterable
  | Height Int
  | Visible Visibility
  deriving (Show, Read, Eq)

instance ToJSON SpaceProperty where
  toJSON Enterable = Y.String "enterable"
  --toJSON Metallic = Y.String "metallic"
  --toJSON Wooden = Y.String "wooden"
  --toJSON Carvable = Y.String "carvable"
  toJSON (Visible vis) = Y.object ["visible" .= (toJSON vis)]
  toJSON (Height h) = Y.object ["height" .= (toJSON h)]

instance FromJSON SpaceProperty where
  parseJSON (Y.String "enterable") = return Enterable
  --parseJSON (Y.String "metallic") = return Metallic
  --parseJSON (Y.String "wooden") = return Wooden
  --parseJSON (Y.String "carvable") = return Carvable
  parseJSON (Y.Object v) 
    =   isKVPair v "height" Height
    <|> isKVPair v "visible" Visible
    <|> typeMismatch "ObjectProperty" (Y.Object v)
  parseJSON invalid = typeMismatch "ObjectProperty" invalid

  parseJSONList = parseAMAP parseJSON

-- data type indicating the type of a space
-- this includes possible subspace relations
-- and restrictions on what can be contained
type SpaceClass = SpaceClassSt Identifier

data SpaceClassSt a = 
  SpClassSt
    { spClassName :: Identifier
    , contentRestrictions :: [ContentClass]
    , subspaceRelations :: [a]
    , spClassProperties :: [SpaceProperty]
    }
  deriving (Show, Read, Eq)

instance (ToJSON a) => ToJSON (SpaceClassSt a) where
  toJSON spclass = Y.object
    [ "space-class" .= spClassName spclass
    , "content-restrictions" .= toJSONList (contentRestrictions spclass)
    , "relations" .= toJSONList (subspaceRelations spclass)
    , "properties" .= toJSONList (spClassProperties spclass)
    ]

instance (FromJSON a) => FromJSON (SpaceClassSt a) where
  parseJSON (Y.Object v) = SpClassSt
    <$> v .: "space-class"
    <*> v .:? "content-restrictions" .!= []
    <*> v .:? "relations" .!= []
    <*> v .:? "properties" .!= []
  parseJSON invalid = typeMismatch "ObjectClass" invalid






