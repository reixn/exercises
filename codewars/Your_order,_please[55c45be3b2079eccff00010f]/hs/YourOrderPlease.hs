module Codewars.Kata.YourOrderPlease (yourOrderPlease) where

import Data.Char
import Data.List qualified as L

yourOrderPlease :: String -> String
yourOrderPlease =
  L.unwords
    . fmap snd
    . L.sort
    . fmap (\w -> (L.find isDigit w, w))
    . L.words