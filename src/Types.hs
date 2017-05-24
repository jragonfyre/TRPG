--
-- Types.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Types where
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

import BaseTypes
import MaterialTypes
import RelationTypes
import UtilityParsers


data World = World
  { registeredRelations :: M.HashMap Identifier Relation
  , registeredMaterials :: M.HashMap Identifier Material
  , registeredSpaceClasses :: M.HashMap Identifier SpaceClass
  , registeredObjectClasses :: M.HashMap Identifier ObjectClass
  }

data Property
  = Visible Visibility
  deriving (Show, Read, Eq)


-- data type denoting restrictions on content
data ContentClass where
  AllForbidden :: ContentClass
  Satisfies :: [Condition] -> ContentClass
  deriving (Show, Read, Eq)

data ObjectClass =
  ObClass
    { propspecs :: [Property]
    , spaceRelationships :: [Relation]
    }
  deriving (Show, Read, Eq)

data ObjectData = 
  ObData
    { properties :: [Property]
    , spaces :: [Space]
    }
  deriving (Show, Read, Eq)

data Object where
  Object :: ObjectClass -> ObjectData -> Object
  deriving (Show, Read, Eq)

-- data type indicating the type of a space
-- this includes possible subspace relations
-- and restrictions on what can be contained
data SpaceClass = 
  SpClass
    { contentRestrictions :: [ContentClass]
    , subspaceRelationships :: [Relation]
    }
  deriving (Show, Read, Eq)

-- dynamic data associated to a space
data SpaceData =
  SpData
    { contents :: [Object]
    , subspaces :: [Space]
    }
  deriving (Show, Read, Eq)

-- describes a space
data Space where
  -- Space with an identifier and certain relations to subspaces
  NamedSpace :: Identifier -> SpaceClass -> SpaceData -> Space
  -- subspace of some parent space with the relation
  Subspace :: Relation -> SpaceClass -> SpaceData -> Space
  deriving (Show, Read, Eq)

-- NamedSpace




