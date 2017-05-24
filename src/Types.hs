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
import ObjectTypes
import SpaceTypes
import ParseUtilities


data World = World
  { registeredRelations :: M.HashMap Identifier Relation
  , registeredMaterials :: M.HashMap Identifier Material
  , registeredSpaceClasses :: M.HashMap Identifier SpaceClass
  , registeredObjectClasses :: M.HashMap Identifier ObjectClass
  }


-- dynamic data associated to a space
data SpaceData =
  SpData
    { contents :: [Object]
    , subspaces :: [Space]
    , spInstanceProperties :: [SpaceProperty]
    }
  deriving (Show, Read, Eq)

-- describes a space
data Space where
  -- Space with an identifier and certain relations to subspaces
  NamedSpace :: Identifier -> SpaceClass -> SpaceData -> Space
  -- subspace of some parent space with the relation
  Subspace :: Relation -> SpaceClass -> SpaceData -> Space
  deriving (Show, Read, Eq)


data ObjectData = 
  ObData
    { objInstanceProperties :: [ObjectProperty]
    , spaces :: [Space]
    }
  deriving (Show, Read, Eq)

data Object where
  Object :: ObjectClass -> ObjectData -> Object
  deriving (Show, Read, Eq)




