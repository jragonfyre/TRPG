--
-- Types.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Game.Types where
  --(
  --) where

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
import Game.MaterialTypes
import Game.RelationTypes
import Game.ObjectTypes
import Game.SpaceTypes
import ParseUtilities


data World = World
  { registeredRelations :: M.HashMap Identifier Relation
  , registeredMaterials :: M.HashMap Identifier Material
  , registeredSpaceClasses :: M.HashMap Identifier SpaceClass
  , registeredObjectClasses :: M.HashMap Identifier ObjectClass
  }


type Space = SpaceData

-- dynamic data associated to a space
data SpaceData =
  SpData
    { spaceId :: Identifier
    , spaceDescription :: Description
    , spaceClass :: Identifier
    , contents :: [Object]
    , subspaces :: [Space]
    , spaceInstanceProperties :: [SpaceProperty]
    }
  deriving (Show, Read, Eq)

instance FromJSON SpaceData where
  parseJSON (Y.Object v) = SpData 
    <$> v .: "space" 
    <*> v .: "description"
    <*> v .: "space-class"
    <*> v .:? "contents" .!= []
    <*> v .:? "subspaces" .!= []
    <*> v .:? "properties" .!= []

instance ToJSON SpaceData where
  toJSON spdata = Y.object
    [ "space" .= spaceId spdata
    , "description" .= spaceDescription spdata
    , "space-class" .= spaceClass spdata
    , "contents" .= toJSONList (contents spdata)
    , "subspaces" .= toJSONList (subspaces spdata)
    , "properties" .= toJSONList (spaceInstanceProperties spdata)
    ]

{-
-- describes a space
data Space where
  -- Space with an identifier and certain relations to subspaces
  NamedSpace :: Identifier -> SpaceClass -> SpaceData -> Space
  -- subspace of some parent space with the relation
  Subspace :: Relation -> SpaceClass -> SpaceData -> Space
  deriving (Show, Read, Eq)
-}

type Object = ObjectData

data ObjectData = 
  ObData
    { objectId :: Identifier
    , objectDescription :: Description
    , objectClass :: Identifier
    , objectSpaces :: [Space]
    , objectInstanceProperties :: [ObjectProperty]
    }
  deriving (Show, Read, Eq)

instance FromJSON ObjectData where
  parseJSON (Y.Object v) = ObData
    <$> v .: "object"
    <*> v .: "description"
    <*> v .: "object-class"
    <*> v .:? "spaces" .!= []
    <*> v .:? "properties" .!= []

instance ToJSON ObjectData where
  toJSON obdata = Y.object
    [ "object" .= objectId obdata
    , "description" .= objectDescription obdata
    , "object-class" .= objectClass obdata
    , "spaces" .= toJSONList (objectSpaces obdata)
    , "properties" .= toJSONList (objectInstanceProperties obdata)
    ]


{-
data Object where
  Object :: ObjectClass -> ObjectData -> Object
  deriving (Show, Read, Eq)
-}




