--
-- Utils.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Utils
  ( allValues
  ) where


allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]


