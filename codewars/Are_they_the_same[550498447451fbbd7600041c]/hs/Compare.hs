module Codewars.Kata.Compare (comp) where

import Data.List qualified as L

comp :: [Integer] -> [Integer] -> Bool
comp (_ : _) [] = False
comp [] (_ : _) = False
comp as bs = L.sort (fmap (\a -> a * a) as) == L.sort bs