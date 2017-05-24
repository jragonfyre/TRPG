--
-- MaterialTypes.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module MaterialTypes
--  (
--  ) where

import BaseTypes
import ParseUtilities

data Material = Material
  { materialName :: Identifier
  , materialParent :: [Material]
  , materialProperties :: [MaterialProperties]
  }




