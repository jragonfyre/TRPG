--
-- Test.hs
-- Copyright (C) 2017 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Test
  ( unpack
  , getRes
  , encodeString
  , displayYaml
  ) where

import Data.Either (rights)
import Data.ByteString.Char8 (unpack)
import Control.Monad (join)
import Data.Yaml (encode, ToJSON (..))

getRes :: Either a [b] -> [b]
getRes r = join $ rights [r]


encodeString :: (ToJSON a) => a -> String
encodeString = unpack . encode

displayYaml :: (ToJSON a) => a -> IO ()
displayYaml = putStrLn . encodeString


