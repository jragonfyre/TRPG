--
-- Types.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Types where
  --(
  --) where

type Identifier = String

-- data type indicating a relationship between spaces.
data Relation = Relation
  { relationName :: Identifier
  , relationRequires :: [Condition]
  , relationDescriptions :: [String]
  , relationProperties :: [Property]
  }
  deriving (Show, Read, Eq)

data Condition
  = WhenObjectState Identifier
  | HasObjectState Identifier
  | WhenAction Identifier
  | HasRelation Identifier
  deriving (Show, Read, Eq)

data Visibility
  = Full
  | Partial
  | Dependent Condition
  deriving (Show, Read, Eq)

type Reason = Identifier
type Result = Relation

data Property
  = Portal -- subspace or portal
  | Visible Visibility
  | Unstable Reason Result
  deriving (Show, Read, Eq)

-- data type denoting restrictions on content
data ContentClass where
  AllForbidden :: ContentClass
  Satisfies :: [Condition] -> ContentClass
  deriving (Show, Read, Eq)


data ObjectType =
  ObType
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
  Object :: ObjectType -> ObjectData -> Object
  deriving (Show, Read, Eq)

-- data type indicating the type of a space
-- this includes possible subspace relations
-- and restrictions on what can be contained
data SpaceType = 
  SpType
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
  NamedSpace :: Identifier -> SpaceType -> SpaceData -> Space
  -- subspace of some parent space with the relation
  Subspace :: Relation -> SpaceType -> SpaceData -> Space
  deriving (Show, Read, Eq)

-- NamedSpace




