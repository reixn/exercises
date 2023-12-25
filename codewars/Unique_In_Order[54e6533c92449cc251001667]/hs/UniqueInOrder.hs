module UniqueInOrder (uniqueInOrder) where

import Data.List qualified as L

uniqueInOrder :: (Eq a) => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder (x : xs) = x : uniqueInOrder (L.dropWhile (== x) xs)