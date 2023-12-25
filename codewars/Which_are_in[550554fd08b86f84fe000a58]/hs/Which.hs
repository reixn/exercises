module Codewars.Kata.Which (inArray) where

import Data.List qualified as L

nubOrd :: [String] -> [String]
nubOrd [] = []
nubOrd (x : xs) = x : nubOrd (L.dropWhile (== x) xs)

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = (nubOrd . L.sort . filter (\s -> any (s `L.isInfixOf`) a2)) a1