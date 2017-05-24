--
-- BaseTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module BaseTypes where
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

import ParseUtilities

type Identifier = String

data Visibility
  = Full
  | Partial
  | Dependent Condition
  deriving (Show, Read, Eq)

instance FromJSON Visibility where
  parseJSON (Y.Bool True) = return Full
  parseJSON (Y.String "yes") = return Full
  parseJSON (Y.String "partly") = return Partial
  parseJSON (Y.Object v) = do
    mdep <- v .:? "dependent"
    case mdep of
      Just cond ->
        return (Dependent cond)
      Nothing ->
        typeMismatch "Visibility" (Y.Object v)
  parseJSON invalid = typeMismatch "Visibility" invalid

instance ToJSON Visibility where
  toJSON Full = Y.String "yes"
  toJSON Partial = Y.String "partly"
  toJSON (Dependent cond) = Y.object ["dependent" .= (toJSON cond)]


data Condition
  = WhenObjectState Identifier
  | HasObjectState Identifier
  | WhenAction Identifier
  | HasRelation Identifier
  deriving (Show, Read, Eq)


instance FromJSON Condition where
  parseJSON (Y.Object v)
    =   isKVPair v "object-state" WhenObjectState
    <|> isKVPair v "has-state" HasObjectState
    <|> isKVPair v "action" WhenAction
    <|> isKVPair v "relation" HasRelation
    <|> typeMismatch "Condition" (Y.Object v)
  parseJSON invalid = typeMismatch "Condition" invalid
  {-
    mwobjst <- v .:? "object-state"
    mhobjst <- v .:? "has-state"
    maction <- v .:? "action"
    mhrelation <- v .:? "relation"
    let ress = May.catMaybes $ map (\(mid, const) -> fmap const mid)
      [ (mwobjst, WhenObjectState)
      , (mhobjst, HasObjectState)
      , (maction, WhenAction)
      , (mhrelation, HasRelation)
      ]
    in case res of 
      [] -> typeMismatch "Condition" (Object v)
      (a:_) -> return a
      -}
    
instance ToJSON Condition where
  toJSON (WhenObjectState id) = Y.object ["object-state" .= (toJSON id)]
  toJSON (HasObjectState id) = Y.object ["in-state" .= (toJSON id)]
  toJSON (WhenAction id) = Y.object ["action" .= (toJSON id)]
  toJSON (HasRelation id) = Y.object ["relation" .= (toJSON id)]



-- data type denoting restrictions on content
data ContentClass where
  AllForbidden :: ContentClass
  Satisfies :: [Condition] -> ContentClass
  deriving (Show, Read, Eq)

instance ToJSON ContentClass where
  toJSON AllForbidden = Y.String "all-forbidden"
  toJSON (Satisfies conds) = Y.array $ map toJSON conds

instance FromJSON ContentClass where
  parseJSON (Y.String "all-forbidden") = return AllForbidden
  parseJSON (Y.Object v)
    =   isKVPair v "satisfies" Satisfies
    <|> typeMismatch "ContentClass" (Y.Object v)

  parseJSONList = parseAMAP parseJSON



