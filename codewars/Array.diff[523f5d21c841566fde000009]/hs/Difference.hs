module Difference (difference) where

difference :: (Eq a) => [a] -> [a] -> [a]
difference [] _ = []
difference (x : xs) b =
  if x `elem` b
    then difference xs b
    else x : difference xs b